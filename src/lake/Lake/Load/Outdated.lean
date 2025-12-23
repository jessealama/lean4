/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jesse Alama
-/
module

prelude
public import Lean.Data.Json
public import Lake.Config.Env
public import Lake.Config.Dependency
public import Lake.Config.OutFormat
public import Lake.Load.Manifest
import Lake.Reservoir
import Lake.Util.Git
import Lake.Util.Version
import Lake.Util.Log

/-! # Outdated Dependency Detection

This module provides functionality for checking which package dependencies
have updates available, similar to `npm outdated` in Node.js.
-/

open System Lean

namespace Lake

/-- Status of a dependency's version relative to what's available. -/
public inductive DepStatus
  /-- Dependency is up to date (installed version matches available). -/
  | upToDate
  /-- Dependency has an update available. -/
  | outdated (installed : String) (available : String) (constraint? : Option String)
  /-- Dependency is a local path dependency (always considered current). -/
  | localPath
  /-- Could not determine status (e.g., network error, not on Reservoir). -/
  | unknown (reason : String)
  deriving Inhabited, Repr

namespace DepStatus

public protected def toJson : DepStatus → Json
  | .upToDate => Json.mkObj [("status", "upToDate")]
  | .outdated installed available constraint? =>
    let fields : List (String × Json) := [
      ("status", Json.str "outdated"),
      ("installed", Json.str installed),
      ("available", Json.str available)
    ]
    let fields := match constraint? with
      | some c => fields ++ [("constraint", Json.str c)]
      | none => fields
    Json.mkObj fields
  | .localPath => Json.mkObj [("status", "localPath")]
  | .unknown reason => Json.mkObj [("status", "unknown"), ("reason", reason)]

public instance : ToJson DepStatus := ⟨DepStatus.toJson⟩

end DepStatus

/-- Information about a dependency's update status. -/
public structure OutdatedInfo where
  /-- Package name. -/
  name : Name
  /-- Package scope (owner on Reservoir). -/
  scope : String
  /-- Version status. -/
  status : DepStatus
  deriving Inhabited

namespace OutdatedInfo

public protected def toJson (info : OutdatedInfo) : Json :=
  let statusJson := info.status.toJson
  match statusJson with
  | .obj fields =>
    let base : List (String × Json) := [("name", Json.str info.name.toString), ("scope", Json.str info.scope)]
    Json.mkObj <| base ++ fields.toList
  | _ => Json.mkObj [("name", info.name.toString), ("scope", info.scope)]

public instance : ToJson OutdatedInfo := ⟨OutdatedInfo.toJson⟩

/-- Format as text for display. -/
public def toText (info : OutdatedInfo) : String :=
  let nameStr := if info.scope.isEmpty then info.name.toString else s!"{info.scope}/{info.name}"
  match info.status with
  | .upToDate => s!"{nameStr}: up to date"
  | .outdated installed available constraint? =>
    let constraintStr := match constraint? with
      | some c => s!" (constraint: {c})"
      | none => ""
    s!"{nameStr}: {installed} -> {available}{constraintStr}"
  | .localPath => s!"{nameStr}: local path dependency"
  | .unknown reason => s!"{nameStr}: unknown ({reason})"

public instance : QueryText OutdatedInfo := ⟨OutdatedInfo.toText⟩
public instance : QueryJson OutdatedInfo := ⟨OutdatedInfo.toJson⟩

end OutdatedInfo

/-- Parse version specification to extract constraint info. -/
private def parseVersionSpec (ver? : Option String) : Option String × Option VerRange :=
  match ver? with
  | none => (none, none)
  | some ver =>
    if ver.startsWith "git#" then
      (some ver, none)
    else
      match VerRange.parse ver with
      | .ok range => (some ver, some range)
      | .error _ => (some ver, none)

/--
Check if a Reservoir package has an update available.
Compares the installed revision against the latest available version.
-/
public def checkReservoirOutdated
  (lakeEnv : Env) (scope : String) (name : Name)
  (installedRev : String) (constraint? : Option String) (range? : Option VerRange)
: LogIO DepStatus := do
  let nameStr := name.toString (escape := false)
  match (← Reservoir.fetchPkgVersions lakeEnv scope nameStr |>.toLogT) with
  | .ok versions =>
    if versions.isEmpty then
      return .unknown "no versions found on Reservoir"
    -- Find the latest version that matches the constraint (if any)
    let matchingVersions := match range? with
      | some range => versions.filter fun (v : RegistryVer) => range.test v.version
      | none => versions
    match matchingVersions[0]? with
    | some latest =>
      if installedRev == latest.revision then
        return .upToDate
      else
        return .outdated installedRev latest.revision constraint?
    | none =>
      return .unknown "no matching versions found"
  | .error _ =>
    return .unknown "failed to fetch versions from Reservoir"

/--
Check if a Git dependency (with branch ref) has updates available.
Fetches the remote and compares HEAD against the installed revision.
-/
public def checkGitBranchOutdated
  (wsDir : FilePath) (relPkgsDir : FilePath) (name : Name)
  (installedRev : String) (inputRev? : Option String)
: LogIO DepStatus := do
  let nameStr := name.toString (escape := false)
  let repoDir := wsDir / relPkgsDir / nameStr
  let repo := GitRepo.mk repoDir
  if !(← repo.dirExists) then
    return .unknown "repository not found"
  -- Only check for updates if inputRev is a branch (not a full SHA or tag)
  match inputRev? with
  | none =>
    -- No input rev specified, check default branch
    match (← repo.findRemoteRevision none |>.toLogT) with
    | .ok remoteRev =>
      if installedRev == remoteRev then
        return .upToDate
      else
        return .outdated installedRev remoteRev none
    | .error _ =>
      return .unknown "failed to fetch remote"
  | some inputRev =>
    -- If it looks like a full SHA, it's pinned - report as up to date
    if Git.isFullObjectName inputRev then
      return .upToDate
    -- Otherwise check if there are newer commits on this ref
    match (← repo.findRemoteRevision inputRev |>.toLogT) with
    | .ok remoteRev =>
      if installedRev == remoteRev then
        return .upToDate
      else
        return .outdated installedRev remoteRev (some s!"git#{inputRev}")
    | .error _ =>
      return .unknown "failed to fetch remote"

/--
Check a single dependency for updates.
-/
public def checkDepOutdated
  (lakeEnv : Env) (wsDir : FilePath) (relPkgsDir : FilePath)
  (entry : PackageEntry) (dep? : Option Dependency)
: LogIO OutdatedInfo := do
  let name := entry.name
  let scope := entry.scope
  match entry.src with
  | .path _ =>
    return { name, scope, status := .localPath }
  | .git _url rev inputRev? _subDir? =>
    -- Check if this is a Reservoir package (has scope)
    if !scope.isEmpty then
      let (constraint?, range?) := match dep? with
        | some dep => parseVersionSpec dep.version?
        | none => (none, none)
      let status ← checkReservoirOutdated lakeEnv scope name rev constraint? range?
      return { name, scope, status }
    else
      -- Plain git dependency - check for branch updates
      let status ← checkGitBranchOutdated wsDir relPkgsDir name rev inputRev?
      return { name, scope, status }

/--
Check all dependencies in a manifest for updates.
Returns an array of `OutdatedInfo` for each dependency.
-/
public def checkAllOutdated
  (lakeEnv : Env) (wsDir : FilePath) (manifest : Manifest)
  (deps : Array Dependency) (filter : NameSet := {})
: LogIO (Array OutdatedInfo) := do
  let relPkgsDir := manifest.packagesDir?.getD defaultPackagesDir
  let depMap := deps.foldl (init := mkNameMap Dependency) fun m d => m.insert d.name d
  let entries := if filter.isEmpty then
    manifest.packages
  else
    manifest.packages.filter (filter.contains ·.name)
  entries.mapM fun entry => do
    checkDepOutdated lakeEnv wsDir relPkgsDir entry (depMap.find? entry.name)

end Lake
