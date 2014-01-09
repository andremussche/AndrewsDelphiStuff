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
  Node_Static,
  socket.io;

{ TServer}

procedure TServer.Run;
begin
  var fileserver := TNodeStaticServer.Create('./public');

  //start http server
  var server: JServer := http.createServer(
    procedure(request: JServerRequest; response: JServerResponse)
    begin
      Console.log('http request: ' + request.url);
      if request.url = '/' then
        response.end('Hello World!')
      else
        fileserver.serve(request, response);
    end);

  var port := 5000;
  if Process.env.PORT > 0 then
    port := Process.env.PORT;
  server.listen(port, '');
  Console.log('Server running at http://127.0.0.1:' + port.ToString);

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
