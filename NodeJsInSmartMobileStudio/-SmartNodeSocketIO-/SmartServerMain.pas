unit SmartServerMain;

interface

type
  TServer = class
  public
    procedure Run;
  end;

implementation

uses
  NodeJS.Core, NodeJS.http,
  socket.io;

{ TServer}

procedure TServer.Run;
begin
  //start http server
  var server: JServer := http.createServer(
    procedure(request: JServerRequest; response: JServerResponse)
    begin
      response.end('Hello World!')
    end);
  server.listen(80, '');
  Console.log('Server running at http://127.0.0.1:80/');

  var value := 0;
  var io := socketio().listen(server);
  io.sockets.on('connection',                //wait for connections
    procedure(socket: JSocketIO)
    begin
      socket.emit('dataPushed', ['test']);   //push some test data to client on connection
      socket.on('requestFromClient',         //wait for a special request from the client
        procedure(data: Variant; callback: JSocketIODataFunction)
        begin
          Console.log('Received from client: ' + data);
          value += data;
          if value > 100 then value := 0;
          callback(value);                   //send some data back

          io.sockets.emit('dataFromServer', [value]);  //send to all clients
        end);
    end);
end;

end.
