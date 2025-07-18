/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Sofia Rodrigues
-/
module

prelude
public import Std.Time
public import Std.Internal.UV.UDP
public import Std.Internal.Async.Select
public import Std.Net.Addr

public section

namespace Std
namespace Internal
namespace IO
namespace Async
namespace UDP

open Std.Net

/--
Represents a UDP socket.
-/
structure Socket where
  private ofNative ::
    native : Internal.UV.UDP.Socket

/--
Membership type for multicast operations.
-/
inductive Membership
  | leaveGroup
  | enterGroup

namespace Socket

/--
Creates a new UDP socket.
-/
@[inline]
def mk : IO Socket := do
  let native ← Internal.UV.UDP.Socket.new
  return Socket.ofNative native

/--
Binds the UDP socket to the given address. Address reuse is enabled to allow rebinding the
same address.
-/
@[inline]
def bind (s : Socket) (addr : SocketAddress) : IO Unit :=
  s.native.bind addr

/--
Associates the UDP socket with the given address and port, so every message sent by this socket is
automatically sent to that destination.
-/
@[inline]
def connect (s : Socket) (addr : SocketAddress) : IO Unit :=
  s.native.connect addr

/--
Sends data through an UDP socket. The `addr` parameter specifies the destination address. If `addr`
is `none`, the data is sent to the default peer address set by `connect`.
-/
@[inline]
def send (s : Socket) (data : ByteArray) (addr : Option SocketAddress := none) : IO (AsyncTask Unit) :=
  AsyncTask.ofPromise <$> s.native.send data addr

/--
Receives data from an UDP socket. `size` is for the maximum bytes to receive.
The promise resolves when some data is available or an error occurs. If the socket
has not been previously bound with `bind`, it is automatically bound to `0.0.0.0`
(all interfaces) with a random port.
Furthermore calling this function in parallel with `recvSelector` is not supported.
-/
@[inline]
def recv (s : Socket) (size : UInt64) : IO (AsyncTask (ByteArray × Option SocketAddress)) :=
  AsyncTask.ofPromise <$> s.native.recv size

/--
Creates a `Selector` that resolves once `s` has data available, up to at most `size` bytes,
and provides that data. If the socket has not been previously bound with `bind`, it is
automatically bound to `0.0.0.0` (all interfaces) with a random port.
Calling this function starts the data wait, so it must not be called in parallel with `recv`.
-/
def recvSelector (s : Socket) (size : UInt64) :
    IO (Selector (ByteArray × Option SocketAddress)) := do
  let readableWaiter ← s.native.waitReadable
  return {
    tryFn := do
      if ← readableWaiter.isResolved then
        -- We know that this read should not block
        let res ← (← s.recv size).block
        return some res
      else
        return none
    registerFn waiter := do
      -- If we get cancelled the promise will be dropped so prepare for that
      discard <| IO.mapTask (t := readableWaiter.result?) fun res => do
        match res with
        | none => return ()
        | some res =>
          let lose := return ()
          let win promise := do
            try
              discard <| IO.ofExcept res
              -- We know that this read should not block
              let res ← (← s.recv size).block
              promise.resolve (.ok res)
            catch e =>
              promise.resolve (.error e)
          waiter.race lose win
    unregisterFn := s.native.cancelRecv
  }

/--
Gets the local address of the UDP socket.
-/
@[inline]
def getSockName (s : Socket) : IO SocketAddress :=
  s.native.getSockName

/--
Gets the remote address of the UDP socket. On unconnected handles, it throws the `.invalidArgument`.
error.
-/
@[inline]
def getPeerName (s : Socket) : IO SocketAddress :=
  s.native.getPeerName

/--
Enables or disables broadcasting for the UDP socket.
-/
@[inline]
def setBroadcast (s : Socket) (enable : Bool) : IO Unit :=
  s.native.setBroadcast enable

/--
Enables or disables multicast loopback for the UDP socket.
-/
@[inline]
def setMulticastLoop (s : Socket) (enable : Bool) : IO Unit :=
  s.native.setMulticastLoop enable

/--
Sets the time-to-live (TTL) for multicast packets.
-/
@[inline]
def setMulticastTTL (s : Socket) (ttl : UInt32) : IO Unit :=
  s.native.setMulticastTTL ttl

/--
Sets the membership for joining or leaving a multicast group.
-/
@[inline]
def setMembership (s : Socket) (multicastAddr : IPAddr) (interfaceAddr : Option IPAddr) (membership : Membership) : IO Unit :=
  let membership := match membership with
    | .leaveGroup => 0
    | .enterGroup => 1
  s.native.setMembership multicastAddr interfaceAddr membership

/--
Sets the multicast interface for sending packets.
-/
@[inline]
def setMulticastInterface (s : Socket) (interfaceAddr : IPAddr) : IO Unit :=
  s.native.setMulticastInterface interfaceAddr

/--
Sets the TTL for outgoing packets.
-/
@[inline]
def setTTL (s : Socket) (ttl : UInt32) : IO Unit :=
  s.native.setTTL ttl

end Socket

end UDP
end Async
end IO
end Internal
end Std
