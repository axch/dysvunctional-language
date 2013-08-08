function fol_program(stdlib, foreign, heap) {
  "use asm";
  var heap_view = new stdlib.Float32Array(heap);
  var acos = stdlib.Math.acos;
  var asin = stdlib.Math.asin;
  var atan = stdlib.Math.atan2;
  var cos = stdlib.Math.cos;
  var sin = stdlib.Math.sin;
  var tan = stdlib.Math.tan;
  var exp = stdlib.Math.exp;
  var log = stdlib.Math.log;
  var sqrt = stdlib.Math.sqrt;
  var expt = stdlib.Math.pow;
  var abs = stdlib.Math.abs;
  function real(x) {
    x = +x;
    return +x;
  }
  function operation_231(the_formals_6902, the_formals_6903, the_formals_6904,
                         the_formals_7164, the_formals_7165) {
    the_formals_6902 = (+the_formals_6902);
    the_formals_6903 = (+the_formals_6903);
    the_formals_6904 = (+the_formals_6904);
    the_formals_7164 = (+the_formals_7164);
    the_formals_7165 = (+the_formals_7165);
    if ((the_formals_6902<=0.0)) {
      heap_view[0] = the_formals_7164;
      heap_view[1] = the_formals_7165;
      return;
    } else {
      return operation_231((the_formals_6902+-1.0), the_formals_6903,
                           the_formals_6904,
                           (((the_formals_7164*the_formals_7164)-(the_formals_7165*the_formals_7165))+the_formals_6903),
                           (((the_formals_7164*the_formals_7165)+(the_formals_7165*the_formals_7164))+the_formals_6904));
    }
  }
  function __main__(x, y) {
    x = +x;
    y = +y;
    var the_formals_11843 = 0.0;
    var the_formals_11844 = 0.0;
    operation_231(400.0, x, y, 0.0, 0.0);
    the_formals_11843 = +heap_view[0];
    the_formals_11844 = +heap_view[1];
    return ((sqrt(((the_formals_11843*the_formals_11843)+(the_formals_11844*the_formals_11844)))<2.0)|0);
  }
  return __main__;
}
