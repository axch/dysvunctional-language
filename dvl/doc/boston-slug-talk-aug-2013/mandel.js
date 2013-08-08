function fol_program(stdlib, foreign, heap) {
  "use asm";
  var heap_view = new stdlib.Float32Array(heap);
  var sqrt = stdlib.Math.sqrt;
  function op_231(count, c_x, c_y, z_x, z_y) {
    count = +count;
    c_x = +c_x;
    c_y = +c_y;
    z_x = +z_x;
    z_y = +z_y;
    if (count <= 0.0) {
      heap_view[0] = z_x;
      heap_view[1] = z_y;
      return;
    } else {
      return op_231(count - 1.0, c_x, c_y,
                    ((z_x*z_x - z_y*z_y) + c_x),
                    ((z_x*z_y + z_y*z_x) + c_y));
    }
  }

  function __main__(x, y) {
    x = +x;
    y = +y;
    var z_x = 0.0;
    var z_y = 0.0;
    op_231(400.0, x, y, 0.0, 0.0);
    z_x = +heap_view[0];
    z_y = +heap_view[1];
    return (sqrt(z_x*z_x + z_y*z_y) < 2.0)|0;
  }
  return __main__;
}
