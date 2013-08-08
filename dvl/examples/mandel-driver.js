/// ----------------------------------------------------------------------
/// Copyright 2013 Alexey Radul.
/// ----------------------------------------------------------------------
/// This file is part of DysVunctional Language.
/// 
/// DysVunctional Language is free software; you can redistribute it and/or modify
/// it under the terms of the GNU Affero General Public License as
/// published by the Free Software Foundation, either version 3 of the
///  License, or (at your option) any later version.
/// 
/// DysVunctional Language is distributed in the hope that it will be useful,
/// but WITHOUT ANY WARRANTY; without even the implied warranty of
/// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/// GNU General Public License for more details.
/// 
/// You should have received a copy of the GNU Affero General Public License
/// along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
/// ----------------------------------------------------------------------

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
    console.time("compute loop");
    var aliasDepth = 2;
    for (var i = 0; i < width; i++) {
        var x = 3*(+i)/width - 2.0;
        var dx = 1.0/(aliasDepth*width);
        for (var j = 0; j < height; j++) {
            var y = 3*(+j)/height - 1.5;
            var dy = 1.0/(aliasDepth*height);
            var ct = 0;
            for (var ii = 0; ii < aliasDepth; ii++) {
                for (var jj = 0; jj < aliasDepth; jj++) {
                    if(mandel(x+ii*dx, y+jj*dy)) {ct = ct + 1;}
                }
            }
            color = 240-(240*ct/(aliasDepth*aliasDepth));
            setPixel(imageData, i, j, color, color, color, 255);
        }
    }
    console.timeEnd("compute loop");
    context.putImageData(imageData, 0, 0);
}
            
