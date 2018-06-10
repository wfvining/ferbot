var websocket;
var server = "ws://" + window.location.host + "/websocket";

var joystick = nipplejs.create({
    zone: document.getElementById('stick'),
    mode: 'static',
    position: { left: '50%', top: '50%' },
    color: '#00e5e5',
    size: 500
});

websocket = new WebSocket(server);

joystick.on('move', function(evt, data){
    var f = Math.exp(Math.log(data.force));
    var x = f * Math.cos(data.angle.radian-(Math.PI/4));
    var y = f * Math.sin(data.angle.radian-(Math.PI/4));
    console.log(data.angle.radian);
    console.log("force: "+(data.force/100));
    command = "d"+x+","+y;
    console.log(command);
    websocket.send(command);
});

joystick.on('end', function(evt, data) {
    websocket.send("d0.0,0.0");
})


