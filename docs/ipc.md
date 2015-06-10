IPC in Hos
==========

Hos implements a robust message-passing system between processes. This messaging system allows processes to send page-aligned messages between processes in a safe manner. Hos IPC uses the underlying architecture's paging mechanisms to achieve quick and timely delivery of messages. The Hos IPC messaging system specifically prohibits any kind of shared memory or state.

Channels
---------

The primary abstraction of the IPC system is that of a channel. A channel is a synchronous message queue identified by a 31-bit identifier. Once a process delivers a message over a channel, it cannot send another message until a reply is received on that channel. Asynchronicity in messaging is achieved through using multiple channels to deliver messages.

IPC system calls
------------------

The Hos kernel allows a process to send a message to any of its children or its parent, but a process can reply to a message sent from any Hos process, regardless of its position in the hierarchy. Processes that receive a message can choose to respond to it immediately, or pass the message on either its parent or one of its children.

The system calls for interacting with the IPC subsystem of the kernel are in the 0x1xx range. The most important IPC system calls are the following, explained in more detail later on:

  * DeliverMessage (0x100)
  * RouteMessage (0x101)
  * ReplyToMessage (0x102)
  * WaitOnChannels (0x103)

### DeliverMessage

The DeliverMessage(from-chan-id, chan-id, recipient-id) system call delivers all pages currently mapped to the given from-chan-id channel to the process specified. The process must either be one of the current process's children or the process's parent. If recipient-id is set to 0, the message is automatically delivered to the parent. The system call will return the number of pages sent, or 0 if there were no pages corresponding to the given channel mapped in the current proccess's address space. An error code is returned if an attempt was made to deliver messages over a channel which has already sent a message with no response

After the call is successful, all mappings that reference the given channel will be removed from the address space.

### RouteMessage

The RouteMessage(from-chan-id, chan-id, recipient-id) system call delivers all pages currently mapped to the given from-chan-id channel to the process specified, which must be either one of the current proccess's children or its parent. The key difference between this and DeliverMessage is the handling of replies to the given message. A reply to a message delivered by DeliverMessage will be directed to the process that called DeliverMessage, while the reply to a message delivered by RouteMessage will be directed to the process that *originally* called DeliverMessage to send the message. Like DeliverMessage, the RouteMessage call removes all mappings belonging to chan-id.

The call returns the number of pages delivered, or 0 on error.

### ReplyToMessage

The ReplyToMessage(to-chan-id) delivers all pages currently mapped to REPLY_FOR(to-chan-id) to the process that originated the message currently in to-chan-id. The call returns the number of pages delivered, or 0 on error.

After the call is complete, all mappings that reference to-chan-id and REPLY_FOR(to-chan-id) are removed from the address space.

### WaitOnChannels

The WaitOnChannels(chan-id-ptr, count, flags, ret-sender-task-id, timeout) system call suspends the current process until one of the given channels receives a message (either a reply or a regular messag). On success, the lower half of the return value gives the the channel id that received a message and the task id pointed to by ret-sender-task-id is filled with the task id of the task that originated the message (or that responded to the message). The higher half of the return value indicates the status. This is a bitset consisting of the following:

  * `HOS_WAIT_ON_CHANNELS_ERROR_STATUS` (0x1) -- Indicates that an error occured
  * `HOS_WAIT_ON_CHANNELS_MSG_TRUNCATED_STATUS` (0x2) -- Indicates that the message was truncated on delivery. This happens if there is no mapping in the address space for one of the channel ids, or if the virtual address range assigned to the mapping is not large enough to hold the message sent. In this case, the remaining pages are thrown out, and cannot be recovered by the current process. They will however be transferred in case ofa routemessage. User software is encouraged to explicitly indicate the maximum size of a response in any message between processes.

  The other bits are reserved for future use, and should be ignored by processes.

On error (`HOS_WAIT_ON_CHANNELS_ERROR_STATUS` is set in the higher word), the lower word contains an indication of the failure, and the value of ret-sender-task-id depends on the exact failure that occured. The current failure indicators are

  * `HOS_WAIT_ON_CHANNELS_NO_MESSAGES_ERROR`(0x10300) -- Indicates that no messages were ready and the timer timed out. This is never returned if the `HOS_WAIT_ON_CHANNELS_WAIT_FOREVER_FLAG` is set.
  * HOS_WAIT_ON_CHANNELS_MSG_TRUNCATED_ERROR(0x10301) -- Indicates that a reply is ready but the message would have to be truncated to fit, and the HOS_WAIT_ON_CHANNEL_DONT_TRUNCATE_FLAG is set. ret-sender-task-id is set to the channel id of the channel that has a message waiting. If the 32nd bit is set, then the message waiting is a reply
  * HOS_ERROR_ACCESS_VIOLATION(0xFFFFFFFF) -- Indicates that the process does not have the authorization to access the area of memory pointed to by ret-sender-task-id or chan-id-ptr or that the memory areas are invalid.
  * HOS_ERROR_INVALID_ARGUMENT(0xFFFFFFFE) -- Indicates that one of the channel pointers in chan-id-ptr had its highest bit set.

The flags argument is a bitset of the following:

  * HOS_WAIT_ON_CHANNELS_WAIT_FOREVER_FLAG (0x1) -- Ignore the value of timeout, and suspend the process forever
  * HOS_WAIT_ON_CHANNELS_DONT_TRUNCATE_FLAG (0x2) -- never return the HOS_WAIT_ON_CHANNEL_MSG_TRUNCATED_STATUS indicator. Instead return the HOS_WAIT_ON_CHANNELS_MSG_TRUNCATED_ERROR. This has the benefit of not removing the message from the channel, and thus keeping it in memory until its received.
