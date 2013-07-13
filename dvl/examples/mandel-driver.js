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
            
