function getRandomInt(min, max) {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min) + min); // The maximum is exclusive and the minimum is inclusive
}

const socket = new WebSocket(
    "ws://localhost:8080/pixel",
    []
);

var hasReceivedState = false;

socket.onopen = function (event) {
    const array = new Uint8Array(1);
    socket.send(array);
};

socket.onmessage = function (event) {
    const data = event.data;
    const reader = new FileReader();
    reader.onload = function() {
        const arrayBuffer = reader.result;
        const array = new Uint16Array(arrayBuffer);
        if (hasReceivedState) {
            drawPixel(array[0], array[1], array[2]);
        } else {
            for (let i = 0; i < array.length; i += 3) {
                console.log(`${array[i]}, ${array[i+1]}: ${array[i+2]}`);
                drawPixel(array[i], array[i+1], array[i+2]);
            }
            hasReceivedState = true;
        }
        
    }
    reader.readAsArrayBuffer(data);
}

// Canvas
let grid = {
    width:64,
    height:64,
}

const canvasWidth = 1024;
const canvasHeight = 1024;

let canvas = document.createElement("CANVAS");
let ctx = canvas.getContext("2d");
canvas.width = canvasWidth;
canvas.height = canvasHeight;
canvas.style.border = "1px solid black"

ctx.beginPath();
ctx.lineWidth = 1;
ctx.lineStyle = "black";

const pixelSize = canvasHeight / grid.height;

for(let x = 0; x < canvasHeight; x += pixelSize){
    ctx.moveTo(x, 0)
    ctx.lineTo(x, canvasHeight)
}
for(let y = 0; y < canvasWidth; y += canvasWidth / grid.width){
    ctx.moveTo(0, y)
    ctx.lineTo(canvasWidth, y)
}
ctx.stroke();
document.body.appendChild(canvas);

canvas.onclick = function (event) {
    const rect = canvas.getBoundingClientRect();
    const x = Math.floor((event.clientX - rect.left) / pixelSize);
    const y = Math.floor((event.clientY - rect.top) / pixelSize);

    sendDrawPixel(x, y, 0);
}

function sendDrawPixel(x, y, color) {
    const binaryData = new Uint16Array([x, y, color]);
    socket.send(
        binaryData
    );

    drawPixel(x, y, color);
}

function drawPixel(x, y, color) {
    ctx.fillStyle = `rgb(${color}, ${color}, ${color})`;
    ctx.fillRect(x * pixelSize, y * pixelSize, pixelSize, pixelSize);
}