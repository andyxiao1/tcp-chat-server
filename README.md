# Messaging System

Andy Xiao (andyxiao)

Prachi Nawathe (nawathe)

Main Files: Server.hs, Client.hs, TUI.hs, Tests.hs

## Files

### Server.hs

State is stored using `TVar` and a couple `Map`s. Messages are communicated between clients/threads using `TChan`. We make use of the STM monad to abstract IO logic away from state logic.

`main` creates a new connection socket to listen for new connections. It also initializes the state and starts the main loop `connLoop`.

`connLoop` listens for new connections on the connection socket. With each new connection, a new socket is created and a thread is spawned to handle back and forth requests on that socket (`clientLoop`).

`clientLoop` handles a single client connections. It spawns a thread to listen for new messages on the client's room/thread channel. On the main thread, it waits for new messages from the client and handles them by calling `handleInput`. Since some commands require responses, it then sends back some responses to the client.

`handleInput` handles a message from the client and updates the state accordingly + returns any responses in an `STM [Response]`. Commands are given with colon notation (for example `:s newroom` or `:g`).

### Client.hs

This file mainly contains the network connection logic and then starts the brick app. The code for the `brick` app is in `TUI.hs`

`main` gets some user data, creates a connection socket to the server, and starts the `brick` terminal UI. It also spawns 2 threads: one to listen for messages from the server and then send them to the `brick` app event handler (through `BChan`) and another to periodically (every 5 seconds) send an event to tell the brick app to pull the newest room list.

### TUI.hs

State contains all the rooms, messages for the user's current room/thread, user name, a form for user input, and a socket connection to the server.

The draw functions create 3 main widgets: one to list all the rooms, one to list all of the messages, and one for the user to input messages.

The event handler can be broken up into 3 parts. First, keyboard events such as `ESC` to quit and `Enter` to send a message using the socket. Second, network listener events which can be either a clear screen event, update room list event, or a normal message event. Third, form events when the user types in the message input widget.

### Tests.hs

Since we ran into difficulty with abstracting all the pure functionality (since our state had `TChan` components requiring the `STM` monad), we created our own unit tests to check the state within the `STM` monad.

## Libraries

We used `brick` which also required the use of `vty` and `microlens-platform`. We also used the `STM` monad in `stm`.
