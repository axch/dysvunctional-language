var mandel = fol_program(window, "foo", new ArrayBuffer(4096));

function setPixel(imageData, x, y, r, g, b, a) {
    index = (x + y * imageData.width) * 4;
    imageData.data[index+0] = r;
    imageData.data[index+1] = g;
    imageData.data[index+2] = b;
    imageData.data[index+3] = a;
}

function doit() {
    canvas = document.getElementById("it");
    context = canvas.getContext("2d");
    width = canvas.width;
    height = canvas.height;
    imageData = context.createImageData(width, height);
    for (var i = 0; i < width; i++) {
        for (var j = 0; j < height; j++) {
            if(mandel(3*(+i)/width - 1.5, 3*(+j)/height - 1.5)) {
                setPixel(imageData, i, j, 0, 0, 0, 255);
            } else {
                setPixel(imageData, i, j, 0, 0, 255, 255);
            }
        }
    }
    context.putImageData(imageData, 0, 0);
}
            
