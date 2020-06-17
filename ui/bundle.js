(function () {
  'use strict';

  var out_of_memory = /* tuple */[
    "Out_of_memory",
    0
  ];

  var sys_error = /* tuple */[
    "Sys_error",
    -1
  ];

  var failure = /* tuple */[
    "Failure",
    -2
  ];

  var invalid_argument = /* tuple */[
    "Invalid_argument",
    -3
  ];

  var end_of_file = /* tuple */[
    "End_of_file",
    -4
  ];

  var division_by_zero = /* tuple */[
    "Division_by_zero",
    -5
  ];

  var not_found = /* tuple */[
    "Not_found",
    -6
  ];

  var match_failure = /* tuple */[
    "Match_failure",
    -7
  ];

  var stack_overflow = /* tuple */[
    "Stack_overflow",
    -8
  ];

  var sys_blocked_io = /* tuple */[
    "Sys_blocked_io",
    -9
  ];

  var assert_failure = /* tuple */[
    "Assert_failure",
    -10
  ];

  var undefined_recursive_module = /* tuple */[
    "Undefined_recursive_module",
    -11
  ];

  out_of_memory.tag = 248;

  sys_error.tag = 248;

  failure.tag = 248;

  invalid_argument.tag = 248;

  end_of_file.tag = 248;

  division_by_zero.tag = 248;

  not_found.tag = 248;

  match_failure.tag = 248;

  stack_overflow.tag = 248;

  sys_blocked_io.tag = 248;

  assert_failure.tag = 248;

  undefined_recursive_module.tag = 248;
  /*  Not a pure module */

  function caml_array_sub(x, offset, len) {
    var result = new Array(len);
    var j = 0;
    var i = offset;
    while(j < len) {
      result[j] = x[i];
      j = j + 1 | 0;
      i = i + 1 | 0;
    }  return result;
  }

  function caml_array_get(xs, index) {
    if (index < 0 || index >= xs.length) {
      throw [
            invalid_argument,
            "index out of bounds"
          ];
    }
    return xs[index];
  }

  function caml_make_vect(len, init) {
    var b = new Array(len);
    for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      b[i] = init;
    }
    return b;
  }
  /* No side effect */

  function app(_f, _args) {
    while(true) {
      var args = _args;
      var f = _f;
      var init_arity = f.length;
      var arity = init_arity === 0 ? 1 : init_arity;
      var len = args.length;
      var d = arity - len | 0;
      if (d === 0) {
        return f.apply(null, args);
      } else if (d < 0) {
        _args = caml_array_sub(args, arity, -d | 0);
        _f = f.apply(null, caml_array_sub(args, 0, arity));
        continue ;
      } else {
        return (function(f,args){
        return function (x) {
          return app(f, args.concat([x]));
        }
        }(f,args));
      }
    }}

  function curry_1(o, a0, arity) {
    switch (arity) {
      case 1 :
          return o(a0);
      case 2 :
          return (function (param) {
              return o(a0, param);
            });
      case 3 :
          return (function (param, param$1) {
              return o(a0, param, param$1);
            });
      case 4 :
          return (function (param, param$1, param$2) {
              return o(a0, param, param$1, param$2);
            });
      case 5 :
          return (function (param, param$1, param$2, param$3) {
              return o(a0, param, param$1, param$2, param$3);
            });
      case 6 :
          return (function (param, param$1, param$2, param$3, param$4) {
              return o(a0, param, param$1, param$2, param$3, param$4);
            });
      case 7 :
          return (function (param, param$1, param$2, param$3, param$4, param$5) {
              return o(a0, param, param$1, param$2, param$3, param$4, param$5);
            });
      default:
        return app(o, [a0]);
    }
  }

  function _1(o, a0) {
    var arity = o.length;
    if (arity === 1) {
      return o(a0);
    } else {
      return curry_1(o, a0, arity);
    }
  }

  function curry_2(o, a0, a1, arity) {
    switch (arity) {
      case 1 :
          return app(o(a0), [a1]);
      case 2 :
          return o(a0, a1);
      case 3 :
          return (function (param) {
              return o(a0, a1, param);
            });
      case 4 :
          return (function (param, param$1) {
              return o(a0, a1, param, param$1);
            });
      case 5 :
          return (function (param, param$1, param$2) {
              return o(a0, a1, param, param$1, param$2);
            });
      case 6 :
          return (function (param, param$1, param$2, param$3) {
              return o(a0, a1, param, param$1, param$2, param$3);
            });
      case 7 :
          return (function (param, param$1, param$2, param$3, param$4) {
              return o(a0, a1, param, param$1, param$2, param$3, param$4);
            });
      default:
        return app(o, [
                    a0,
                    a1
                  ]);
    }
  }

  function _2(o, a0, a1) {
    var arity = o.length;
    if (arity === 2) {
      return o(a0, a1);
    } else {
      return curry_2(o, a0, a1, arity);
    }
  }

  function curry_3(o, a0, a1, a2, arity) {
    switch (arity) {
      case 1 :
          return app(o(a0), [
                      a1,
                      a2
                    ]);
      case 2 :
          return app(o(a0, a1), [a2]);
      case 3 :
          return o(a0, a1, a2);
      case 4 :
          return (function (param) {
              return o(a0, a1, a2, param);
            });
      case 5 :
          return (function (param, param$1) {
              return o(a0, a1, a2, param, param$1);
            });
      case 6 :
          return (function (param, param$1, param$2) {
              return o(a0, a1, a2, param, param$1, param$2);
            });
      case 7 :
          return (function (param, param$1, param$2, param$3) {
              return o(a0, a1, a2, param, param$1, param$2, param$3);
            });
      default:
        return app(o, [
                    a0,
                    a1,
                    a2
                  ]);
    }
  }

  function _3(o, a0, a1, a2) {
    var arity = o.length;
    if (arity === 3) {
      return o(a0, a1, a2);
    } else {
      return curry_3(o, a0, a1, a2, arity);
    }
  }

  function curry_4(o, a0, a1, a2, a3, arity) {
    switch (arity) {
      case 1 :
          return app(o(a0), [
                      a1,
                      a2,
                      a3
                    ]);
      case 2 :
          return app(o(a0, a1), [
                      a2,
                      a3
                    ]);
      case 3 :
          return app(o(a0, a1, a2), [a3]);
      case 4 :
          return o(a0, a1, a2, a3);
      case 5 :
          return (function (param) {
              return o(a0, a1, a2, a3, param);
            });
      case 6 :
          return (function (param, param$1) {
              return o(a0, a1, a2, a3, param, param$1);
            });
      case 7 :
          return (function (param, param$1, param$2) {
              return o(a0, a1, a2, a3, param, param$1, param$2);
            });
      default:
        return app(o, [
                    a0,
                    a1,
                    a2,
                    a3
                  ]);
    }
  }

  function _4(o, a0, a1, a2, a3) {
    var arity = o.length;
    if (arity === 4) {
      return o(a0, a1, a2, a3);
    } else {
      return curry_4(o, a0, a1, a2, a3, arity);
    }
  }

  function curry_5(o, a0, a1, a2, a3, a4, arity) {
    switch (arity) {
      case 1 :
          return app(o(a0), [
                      a1,
                      a2,
                      a3,
                      a4
                    ]);
      case 2 :
          return app(o(a0, a1), [
                      a2,
                      a3,
                      a4
                    ]);
      case 3 :
          return app(o(a0, a1, a2), [
                      a3,
                      a4
                    ]);
      case 4 :
          return app(o(a0, a1, a2, a3), [a4]);
      case 5 :
          return o(a0, a1, a2, a3, a4);
      case 6 :
          return (function (param) {
              return o(a0, a1, a2, a3, a4, param);
            });
      case 7 :
          return (function (param, param$1) {
              return o(a0, a1, a2, a3, a4, param, param$1);
            });
      default:
        return app(o, [
                    a0,
                    a1,
                    a2,
                    a3,
                    a4
                  ]);
    }
  }

  function _5(o, a0, a1, a2, a3, a4) {
    var arity = o.length;
    if (arity === 5) {
      return o(a0, a1, a2, a3, a4);
    } else {
      return curry_5(o, a0, a1, a2, a3, a4, arity);
    }
  }

  function curry_6(o, a0, a1, a2, a3, a4, a5, arity) {
    switch (arity) {
      case 1 :
          return app(o(a0), [
                      a1,
                      a2,
                      a3,
                      a4,
                      a5
                    ]);
      case 2 :
          return app(o(a0, a1), [
                      a2,
                      a3,
                      a4,
                      a5
                    ]);
      case 3 :
          return app(o(a0, a1, a2), [
                      a3,
                      a4,
                      a5
                    ]);
      case 4 :
          return app(o(a0, a1, a2, a3), [
                      a4,
                      a5
                    ]);
      case 5 :
          return app(o(a0, a1, a2, a3, a4), [a5]);
      case 6 :
          return o(a0, a1, a2, a3, a4, a5);
      case 7 :
          return (function (param) {
              return o(a0, a1, a2, a3, a4, a5, param);
            });
      default:
        return app(o, [
                    a0,
                    a1,
                    a2,
                    a3,
                    a4,
                    a5
                  ]);
    }
  }

  function _6(o, a0, a1, a2, a3, a4, a5) {
    var arity = o.length;
    if (arity === 6) {
      return o(a0, a1, a2, a3, a4, a5);
    } else {
      return curry_6(o, a0, a1, a2, a3, a4, a5, arity);
    }
  }

  function curry_7(o, a0, a1, a2, a3, a4, a5, a6, arity) {
    switch (arity) {
      case 1 :
          return app(o(a0), [
                      a1,
                      a2,
                      a3,
                      a4,
                      a5,
                      a6
                    ]);
      case 2 :
          return app(o(a0, a1), [
                      a2,
                      a3,
                      a4,
                      a5,
                      a6
                    ]);
      case 3 :
          return app(o(a0, a1, a2), [
                      a3,
                      a4,
                      a5,
                      a6
                    ]);
      case 4 :
          return app(o(a0, a1, a2, a3), [
                      a4,
                      a5,
                      a6
                    ]);
      case 5 :
          return app(o(a0, a1, a2, a3, a4), [
                      a5,
                      a6
                    ]);
      case 6 :
          return app(o(a0, a1, a2, a3, a4, a5), [a6]);
      case 7 :
          return o(a0, a1, a2, a3, a4, a5, a6);
      default:
        return app(o, [
                    a0,
                    a1,
                    a2,
                    a3,
                    a4,
                    a5,
                    a6
                  ]);
    }
  }

  function _7(o, a0, a1, a2, a3, a4, a5, a6) {
    var arity = o.length;
    if (arity === 7) {
      return o(a0, a1, a2, a3, a4, a5, a6);
    } else {
      return curry_7(o, a0, a1, a2, a3, a4, a5, a6, arity);
    }
  }

  function curry_8(o, a0, a1, a2, a3, a4, a5, a6, a7, arity) {
    switch (arity) {
      case 1 :
          return app(o(a0), [
                      a1,
                      a2,
                      a3,
                      a4,
                      a5,
                      a6,
                      a7
                    ]);
      case 2 :
          return app(o(a0, a1), [
                      a2,
                      a3,
                      a4,
                      a5,
                      a6,
                      a7
                    ]);
      case 3 :
          return app(o(a0, a1, a2), [
                      a3,
                      a4,
                      a5,
                      a6,
                      a7
                    ]);
      case 4 :
          return app(o(a0, a1, a2, a3), [
                      a4,
                      a5,
                      a6,
                      a7
                    ]);
      case 5 :
          return app(o(a0, a1, a2, a3, a4), [
                      a5,
                      a6,
                      a7
                    ]);
      case 6 :
          return app(o(a0, a1, a2, a3, a4, a5), [
                      a6,
                      a7
                    ]);
      case 7 :
          return app(o(a0, a1, a2, a3, a4, a5, a6), [a7]);
      default:
        return app(o, [
                    a0,
                    a1,
                    a2,
                    a3,
                    a4,
                    a5,
                    a6,
                    a7
                  ]);
    }
  }

  function _8(o, a0, a1, a2, a3, a4, a5, a6, a7) {
    var arity = o.length;
    if (arity === 8) {
      return o(a0, a1, a2, a3, a4, a5, a6, a7);
    } else {
      return curry_8(o, a0, a1, a2, a3, a4, a5, a6, a7, arity);
    }
  }
  /* No side effect */

  function __(tag, block) {
    block.tag = tag;
    return block;
  }
  /* No side effect */

  function caml_string_compare(s1, s2) {
    if (s1 === s2) {
      return 0;
    } else if (s1 < s2) {
      return -1;
    } else {
      return 1;
    }
  }
  /* No side effect */

  var for_in = (function(o,foo){
          for (var x in o) { foo(x); }});

  function caml_equal(_a, _b) {
    while(true) {
      var b = _b;
      var a = _a;
      if (a === b) {
        return true;
      } else {
        var a_type = typeof a;
        if (a_type === "string" || a_type === "number" || a_type === "boolean" || a_type === "undefined" || a === null) {
          return false;
        } else {
          var b_type = typeof b;
          if (a_type === "function" || b_type === "function") {
            throw [
                  invalid_argument,
                  "equal: functional value"
                ];
          }
          if (b_type === "number" || b_type === "undefined" || b === null) {
            return false;
          } else {
            var tag_a = a.tag | 0;
            var tag_b = b.tag | 0;
            if (tag_a === 250) {
              _a = a[0];
              continue ;
            } else if (tag_b === 250) {
              _b = b[0];
              continue ;
            } else if (tag_a === 248) {
              return a[1] === b[1];
            } else {
              if (tag_a === 251) {
                throw [
                      invalid_argument,
                      "equal: abstract value"
                    ];
              }
              if (tag_a !== tag_b) {
                return false;
              } else if (tag_a === 256) {
                return a[1] === b[1];
              } else {
                var len_a = a.length | 0;
                var len_b = b.length | 0;
                if (len_a === len_b) {
                  if (Array.isArray(a)) {
                    var a$1 = a;
                    var b$1 = b;
                    var _i = 0;
                    var same_length = len_a;
                    while(true) {
                      var i = _i;
                      if (i === same_length) {
                        return true;
                      } else if (caml_equal(a$1[i], b$1[i])) {
                        _i = i + 1 | 0;
                        continue ;
                      } else {
                        return false;
                      }
                    }                } else if ((a instanceof Date && b instanceof Date)) {
                    return !(a > b || a < b);
                  } else {
                    var a$2 = a;
                    var b$2 = b;
                    var result = {
                      contents: true
                    };
                    var do_key_a = (function(b$2,result){
                    return function do_key_a(key) {
                      if (b$2.hasOwnProperty(key)) {
                        return 0;
                      } else {
                        result.contents = false;
                        return /* () */0;
                      }
                    }
                    }(b$2,result));
                    var do_key_b = (function(a$2,b$2,result){
                    return function do_key_b(key) {
                      if (!a$2.hasOwnProperty(key) || !caml_equal(b$2[key], a$2[key])) {
                        result.contents = false;
                        return /* () */0;
                      } else {
                        return 0;
                      }
                    }
                    }(a$2,b$2,result));
                    for_in(a$2, do_key_a);
                    if (result.contents) {
                      for_in(b$2, do_key_b);
                    }
                    return result.contents;
                  }
                } else {
                  return false;
                }
              }
            }
          }
        }
      }
    }}
  /* No side effect */

  function caml_create_bytes(len) {
    if (len < 0) {
      throw [
            invalid_argument,
            "String.create"
          ];
    }
    var result = new Array(len);
    for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      result[i] = /* "\000" */0;
    }
    return result;
  }

  function caml_blit_bytes(s1, i1, s2, i2, len) {
    if (len > 0) {
      if (s1 === s2) {
        var s1$1 = s1;
        var i1$1 = i1;
        var i2$1 = i2;
        var len$1 = len;
        if (i1$1 < i2$1) {
          var range_a = (s1$1.length - i2$1 | 0) - 1 | 0;
          var range_b = len$1 - 1 | 0;
          var range = range_a > range_b ? range_b : range_a;
          for(var j = range; j >= 0; --j){
            s1$1[i2$1 + j | 0] = s1$1[i1$1 + j | 0];
          }
          return /* () */0;
        } else if (i1$1 > i2$1) {
          var range_a$1 = (s1$1.length - i1$1 | 0) - 1 | 0;
          var range_b$1 = len$1 - 1 | 0;
          var range$1 = range_a$1 > range_b$1 ? range_b$1 : range_a$1;
          for(var k = 0; k <= range$1; ++k){
            s1$1[i2$1 + k | 0] = s1$1[i1$1 + k | 0];
          }
          return /* () */0;
        } else {
          return 0;
        }
      } else {
        var off1 = s1.length - i1 | 0;
        if (len <= off1) {
          for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
            s2[i2 + i | 0] = s1[i1 + i | 0];
          }
          return /* () */0;
        } else {
          for(var i$1 = 0 ,i_finish$1 = off1 - 1 | 0; i$1 <= i_finish$1; ++i$1){
            s2[i2 + i$1 | 0] = s1[i1 + i$1 | 0];
          }
          for(var i$2 = off1 ,i_finish$2 = len - 1 | 0; i$2 <= i_finish$2; ++i$2){
            s2[i2 + i$2 | 0] = /* "\000" */0;
          }
          return /* () */0;
        }
      }
    } else {
      return 0;
    }
  }

  function bytes_to_string(a) {
    var bytes = a;
    var len = a.length;
    var s = "";
    var s_len = len;
    if ( len <= 4096 && len === bytes.length) {
      return String.fromCharCode.apply(null, bytes);
    } else {
      var offset = 0;
      while(s_len > 0) {
        var next = s_len < 1024 ? s_len : 1024;
        var tmp_bytes = new Array(next);
        caml_blit_bytes(bytes, offset, tmp_bytes, 0, next);
        s = s + String.fromCharCode.apply(null, tmp_bytes);
        s_len = s_len - next | 0;
        offset = offset + next | 0;
      }    return s;
    }
  }

  function caml_blit_string(s1, i1, s2, i2, len) {
    if (len > 0) {
      var off1 = s1.length - i1 | 0;
      if (len <= off1) {
        for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
          s2[i2 + i | 0] = s1.charCodeAt(i1 + i | 0);
        }
        return /* () */0;
      } else {
        for(var i$1 = 0 ,i_finish$1 = off1 - 1 | 0; i$1 <= i_finish$1; ++i$1){
          s2[i2 + i$1 | 0] = s1.charCodeAt(i1 + i$1 | 0);
        }
        for(var i$2 = off1 ,i_finish$2 = len - 1 | 0; i$2 <= i_finish$2; ++i$2){
          s2[i2 + i$2 | 0] = /* "\000" */0;
        }
        return /* () */0;
      }
    } else {
      return 0;
    }
  }
  /* No side effect */

  var id = {
    contents: 0
  };

  function caml_fresh_oo_id(param) {
    id.contents = id.contents + 1;
    return id.contents;
  }

  function create(str) {
    var v_001 = caml_fresh_oo_id();
    var v = /* tuple */[
      str,
      v_001
    ];
    v.tag = 248;
    return v;
  }

  function caml_is_extension(e) {
    if (e === undefined) {
      return false;
    } else if (e.tag === 248) {
      return true;
    } else {
      var slot = e[0];
      if (slot !== undefined) {
        return slot.tag === 248;
      } else {
        return false;
      }
    }
  }
  /* No side effect */

  var undefinedHeader = [];

  function some(x) {
    if (x === undefined) {
      var block = /* tuple */[
        undefinedHeader,
        0
      ];
      block.tag = 256;
      return block;
    } else if (x !== null && x[0] === undefinedHeader) {
      var nid = x[1] + 1 | 0;
      var block$1 = /* tuple */[
        undefinedHeader,
        nid
      ];
      block$1.tag = 256;
      return block$1;
    } else {
      return x;
    }
  }

  function valFromOption(x) {
    if (x !== null && x[0] === undefinedHeader) {
      var depth = x[1];
      if (depth === 0) {
        return ;
      } else {
        return /* tuple */[
                undefinedHeader,
                depth - 1 | 0
              ];
      }
    } else {
      return x;
    }
  }
  /* No side effect */

  var $$Error = create("Caml_js_exceptions.Error");

  function internalToOCamlException(e) {
    if (caml_is_extension(e)) {
      return e;
    } else {
      return [
              $$Error,
              e
            ];
    }
  }
  /* No side effect */

  var Exit = create("Pervasives.Exit");

  var min_int = -2147483648;

  function $at(l1, l2) {
    if (l1) {
      return /* :: */[
              l1[0],
              $at(l1[1], l2)
            ];
    } else {
      return l2;
    }
  }

  var max_int = 2147483647;
  /* No side effect */

  function rev_append(_l1, _l2) {
    while(true) {
      var l2 = _l2;
      var l1 = _l1;
      if (l1) {
        _l2 = /* :: */[
          l1[0],
          l2
        ];
        _l1 = l1[1];
        continue ;
      } else {
        return l2;
      }
    }}

  function rev(l) {
    return rev_append(l, /* [] */0);
  }

  function map(f, param) {
    if (param) {
      var r = _1(f, param[0]);
      return /* :: */[
              r,
              map(f, param[1])
            ];
    } else {
      return /* [] */0;
    }
  }

  function iter(f, _param) {
    while(true) {
      var param = _param;
      if (param) {
        _1(f, param[0]);
        _param = param[1];
        continue ;
      } else {
        return /* () */0;
      }
    }}

  function fold_left(f, _accu, _l) {
    while(true) {
      var l = _l;
      var accu = _accu;
      if (l) {
        _l = l[1];
        _accu = _2(f, accu, l[0]);
        continue ;
      } else {
        return accu;
      }
    }}

  function fold_right(f, l, accu) {
    if (l) {
      return _2(f, l[0], fold_right(f, l[1], accu));
    } else {
      return accu;
    }
  }

  function fold_left2(f, _accu, _l1, _l2) {
    while(true) {
      var l2 = _l2;
      var l1 = _l1;
      var accu = _accu;
      if (l1) {
        if (l2) {
          _l2 = l2[1];
          _l1 = l1[1];
          _accu = _3(f, accu, l1[0], l2[0]);
          continue ;
        } else {
          throw [
                invalid_argument,
                "List.fold_left2"
              ];
        }
      } else {
        if (l2) {
          throw [
                invalid_argument,
                "List.fold_left2"
              ];
        }
        return accu;
      }
    }}

  function find_all(p) {
    return (function (param) {
        var _accu = /* [] */0;
        var _param = param;
        while(true) {
          var param$1 = _param;
          var accu = _accu;
          if (param$1) {
            var l = param$1[1];
            var x = param$1[0];
            if (_1(p, x)) {
              _param = l;
              _accu = /* :: */[
                x,
                accu
              ];
              continue ;
            } else {
              _param = l;
              continue ;
            }
          } else {
            return rev_append(accu, /* [] */0);
          }
        }    });
  }

  var append = $at;

  var filter = find_all;
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE


  var tz_labels = /* :: */[
    "Africa/Abidjan",
    /* :: */[
      "Africa/Accra",
      /* :: */[
        "Africa/Algiers",
        /* :: */[
          "Africa/Bissau",
          /* :: */[
            "Africa/Cairo",
            /* :: */[
              "Africa/Casablanca",
              /* :: */[
                "Africa/Ceuta",
                /* :: */[
                  "Africa/El_Aaiun",
                  /* :: */[
                    "Africa/Johannesburg",
                    /* :: */[
                      "Africa/Juba",
                      /* :: */[
                        "Africa/Khartoum",
                        /* :: */[
                          "Africa/Lagos",
                          /* :: */[
                            "Africa/Maputo",
                            /* :: */[
                              "Africa/Monrovia",
                              /* :: */[
                                "Africa/Nairobi",
                                /* :: */[
                                  "Africa/Ndjamena",
                                  /* :: */[
                                    "Africa/Sao_Tome",
                                    /* :: */[
                                      "Africa/Tripoli",
                                      /* :: */[
                                        "Africa/Tunis",
                                        /* :: */[
                                          "Africa/Windhoek",
                                          /* :: */[
                                            "America/Adak",
                                            /* :: */[
                                              "America/Anchorage",
                                              /* :: */[
                                                "America/Araguaina",
                                                /* :: */[
                                                  "America/Argentina/Buenos_Aires",
                                                  /* :: */[
                                                    "America/Argentina/Catamarca",
                                                    /* :: */[
                                                      "America/Argentina/Cordoba",
                                                      /* :: */[
                                                        "America/Argentina/Jujuy",
                                                        /* :: */[
                                                          "America/Argentina/La_Rioja",
                                                          /* :: */[
                                                            "America/Argentina/Mendoza",
                                                            /* :: */[
                                                              "America/Argentina/Rio_Gallegos",
                                                              /* :: */[
                                                                "America/Argentina/Salta",
                                                                /* :: */[
                                                                  "America/Argentina/San_Juan",
                                                                  /* :: */[
                                                                    "America/Argentina/San_Luis",
                                                                    /* :: */[
                                                                      "America/Argentina/Tucuman",
                                                                      /* :: */[
                                                                        "America/Argentina/Ushuaia",
                                                                        /* :: */[
                                                                          "America/Asuncion",
                                                                          /* :: */[
                                                                            "America/Atikokan",
                                                                            /* :: */[
                                                                              "America/Bahia",
                                                                              /* :: */[
                                                                                "America/Bahia_Banderas",
                                                                                /* :: */[
                                                                                  "America/Barbados",
                                                                                  /* :: */[
                                                                                    "America/Belem",
                                                                                    /* :: */[
                                                                                      "America/Belize",
                                                                                      /* :: */[
                                                                                        "America/Blanc-Sablon",
                                                                                        /* :: */[
                                                                                          "America/Boa_Vista",
                                                                                          /* :: */[
                                                                                            "America/Bogota",
                                                                                            /* :: */[
                                                                                              "America/Boise",
                                                                                              /* :: */[
                                                                                                "America/Cambridge_Bay",
                                                                                                /* :: */[
                                                                                                  "America/Campo_Grande",
                                                                                                  /* :: */[
                                                                                                    "America/Cancun",
                                                                                                    /* :: */[
                                                                                                      "America/Caracas",
                                                                                                      /* :: */[
                                                                                                        "America/Cayenne",
                                                                                                        /* :: */[
                                                                                                          "America/Chicago",
                                                                                                          /* :: */[
                                                                                                            "America/Chihuahua",
                                                                                                            /* :: */[
                                                                                                              "America/Costa_Rica",
                                                                                                              /* :: */[
                                                                                                                "America/Creston",
                                                                                                                /* :: */[
                                                                                                                  "America/Cuiaba",
                                                                                                                  /* :: */[
                                                                                                                    "America/Curacao",
                                                                                                                    /* :: */[
                                                                                                                      "America/Danmarkshavn",
                                                                                                                      /* :: */[
                                                                                                                        "America/Dawson",
                                                                                                                        /* :: */[
                                                                                                                          "America/Dawson_Creek",
                                                                                                                          /* :: */[
                                                                                                                            "America/Denver",
                                                                                                                            /* :: */[
                                                                                                                              "America/Detroit",
                                                                                                                              /* :: */[
                                                                                                                                "America/Edmonton",
                                                                                                                                /* :: */[
                                                                                                                                  "America/Eirunepe",
                                                                                                                                  /* :: */[
                                                                                                                                    "America/El_Salvador",
                                                                                                                                    /* :: */[
                                                                                                                                      "America/Fort_Nelson",
                                                                                                                                      /* :: */[
                                                                                                                                        "America/Fortaleza",
                                                                                                                                        /* :: */[
                                                                                                                                          "America/Glace_Bay",
                                                                                                                                          /* :: */[
                                                                                                                                            "America/Godthab",
                                                                                                                                            /* :: */[
                                                                                                                                              "America/Goose_Bay",
                                                                                                                                              /* :: */[
                                                                                                                                                "America/Grand_Turk",
                                                                                                                                                /* :: */[
                                                                                                                                                  "America/Guatemala",
                                                                                                                                                  /* :: */[
                                                                                                                                                    "America/Guayaquil",
                                                                                                                                                    /* :: */[
                                                                                                                                                      "America/Guyana",
                                                                                                                                                      /* :: */[
                                                                                                                                                        "America/Halifax",
                                                                                                                                                        /* :: */[
                                                                                                                                                          "America/Havana",
                                                                                                                                                          /* :: */[
                                                                                                                                                            "America/Hermosillo",
                                                                                                                                                            /* :: */[
                                                                                                                                                              "America/Indiana/Indianapolis",
                                                                                                                                                              /* :: */[
                                                                                                                                                                "America/Indiana/Knox",
                                                                                                                                                                /* :: */[
                                                                                                                                                                  "America/Indiana/Marengo",
                                                                                                                                                                  /* :: */[
                                                                                                                                                                    "America/Indiana/Petersburg",
                                                                                                                                                                    /* :: */[
                                                                                                                                                                      "America/Indiana/Tell_City",
                                                                                                                                                                      /* :: */[
                                                                                                                                                                        "America/Indiana/Vevay",
                                                                                                                                                                        /* :: */[
                                                                                                                                                                          "America/Indiana/Vincennes",
                                                                                                                                                                          /* :: */[
                                                                                                                                                                            "America/Indiana/Winamac",
                                                                                                                                                                            /* :: */[
                                                                                                                                                                              "America/Inuvik",
                                                                                                                                                                              /* :: */[
                                                                                                                                                                                "America/Iqaluit",
                                                                                                                                                                                /* :: */[
                                                                                                                                                                                  "America/Jamaica",
                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                    "America/Juneau",
                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                      "America/Kentucky/Louisville",
                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                        "America/Kentucky/Monticello",
                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                          "America/La_Paz",
                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                            "America/Lima",
                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                              "America/Los_Angeles",
                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                "America/Maceio",
                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                  "America/Managua",
                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                    "America/Manaus",
                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                      "America/Martinique",
                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                        "America/Matamoros",
                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                          "America/Mazatlan",
                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                            "America/Menominee",
                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                              "America/Merida",
                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                "America/Metlakatla",
                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                  "America/Mexico_City",
                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                    "America/Miquelon",
                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                      "America/Moncton",
                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                        "America/Monterrey",
                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                          "America/Montevideo",
                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                            "America/Nassau",
                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                              "America/New_York",
                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                "America/Nipigon",
                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                  "America/Nome",
                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                    "America/Noronha",
                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                      "America/North_Dakota/Beulah",
                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                        "America/North_Dakota/Center",
                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                          "America/North_Dakota/New_Salem",
                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                            "America/Ojinaga",
                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                              "America/Panama",
                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                "America/Pangnirtung",
                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                  "America/Paramaribo",
                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                    "America/Phoenix",
                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                      "America/Port-au-Prince",
                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                        "America/Port_of_Spain",
                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                          "America/Porto_Velho",
                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                            "America/Puerto_Rico",
                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                              "America/Punta_Arenas",
                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                "America/Rainy_River",
                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                  "America/Rankin_Inlet",
                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                    "America/Recife",
                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                      "America/Regina",
                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                        "America/Resolute",
                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                          "America/Rio_Branco",
                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                            "America/Santarem",
                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                              "America/Santiago",
                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                "America/Santo_Domingo",
                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                  "America/Sao_Paulo",
                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                    "America/Scoresbysund",
                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                      "America/Sitka",
                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                        "America/St_Johns",
                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                          "America/Swift_Current",
                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                            "America/Tegucigalpa",
                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                              "America/Thule",
                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                "America/Thunder_Bay",
                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                  "America/Tijuana",
                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                    "America/Toronto",
                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                      "America/Vancouver",
                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                        "America/Whitehorse",
                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                          "America/Winnipeg",
                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                            "America/Yakutat",
                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                              "America/Yellowknife",
                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                "Antarctica/Casey",
                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                  "Antarctica/Davis",
                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                    "Antarctica/DumontDUrville",
                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                      "Antarctica/Macquarie",
                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                        "Antarctica/Mawson",
                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                          "Antarctica/Palmer",
                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                            "Antarctica/Rothera",
                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                              "Antarctica/Syowa",
                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                "Antarctica/Troll",
                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                  "Antarctica/Vostok",
                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                    "Asia/Almaty",
                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                      "Asia/Amman",
                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                        "Asia/Anadyr",
                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                          "Asia/Aqtau",
                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                            "Asia/Aqtobe",
                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                              "Asia/Ashgabat",
                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                "Asia/Atyrau",
                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                  "Asia/Baghdad",
                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                    "Asia/Baku",
                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                      "Asia/Bangkok",
                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                        "Asia/Barnaul",
                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                          "Asia/Beirut",
                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                            "Asia/Bishkek",
                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                              "Asia/Brunei",
                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                "Asia/Chita",
                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                  "Asia/Choibalsan",
                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                    "Asia/Colombo",
                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                      "Asia/Damascus",
                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                        "Asia/Dhaka",
                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                          "Asia/Dili",
                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                            "Asia/Dubai",
                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                              "Asia/Dushanbe",
                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                "Asia/Famagusta",
                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                  "Asia/Gaza",
                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                    "Asia/Hebron",
                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                      "Asia/Ho_Chi_Minh",
                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                        "Asia/Hong_Kong",
                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                          "Asia/Hovd",
                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                            "Asia/Irkutsk",
                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                              "Asia/Jakarta",
                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                "Asia/Jayapura",
                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                  "Asia/Jerusalem",
                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                    "Asia/Kabul",
                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                      "Asia/Kamchatka",
                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                        "Asia/Karachi",
                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                          "Asia/Kathmandu",
                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                            "Asia/Khandyga",
                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                              "Asia/Kolkata",
                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                "Asia/Krasnoyarsk",
                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                  "Asia/Kuala_Lumpur",
                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                    "Asia/Kuching",
                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                      "Asia/Macau",
                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                        "Asia/Magadan",
                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                          "Asia/Makassar",
                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                            "Asia/Manila",
                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                              "Asia/Nicosia",
                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                "Asia/Novokuznetsk",
                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                  "Asia/Novosibirsk",
                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                    "Asia/Omsk",
                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                      "Asia/Oral",
                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                        "Asia/Pontianak",
                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                          "Asia/Pyongyang",
                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                            "Asia/Qatar",
                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                              "Asia/Qostanay",
                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                "Asia/Qyzylorda",
                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Asia/Riyadh",
                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Asia/Sakhalin",
                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Asia/Samarkand",
                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Asia/Seoul",
                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Asia/Shanghai",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Asia/Singapore",
                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Asia/Srednekolymsk",
                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Asia/Taipei",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Asia/Tashkent",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Asia/Tbilisi",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Asia/Tehran",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Asia/Thimphu",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Asia/Tokyo",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Asia/Tomsk",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Asia/Ulaanbaatar",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Asia/Urumqi",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Asia/Ust-Nera",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Asia/Vladivostok",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Asia/Yakutsk",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Asia/Yangon",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Asia/Yekaterinburg",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Asia/Yerevan",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Atlantic/Azores",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Atlantic/Bermuda",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Atlantic/Canary",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Atlantic/Cape_Verde",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Atlantic/Faroe",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Atlantic/Madeira",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Atlantic/Reykjavik",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Atlantic/South_Georgia",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Atlantic/Stanley",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Australia/Adelaide",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Australia/Brisbane",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Australia/Broken_Hill",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Australia/Currie",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Australia/Darwin",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Australia/Eucla",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Australia/Hobart",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Australia/Lindeman",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Australia/Lord_Howe",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Australia/Melbourne",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Australia/Perth",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Australia/Sydney",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Etc/GMT",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Etc/GMT+1",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Etc/GMT+10",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Etc/GMT+11",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Etc/GMT+12",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Etc/GMT+2",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Etc/GMT+3",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Etc/GMT+4",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Etc/GMT+5",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Etc/GMT+6",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Etc/GMT+7",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Etc/GMT+8",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Etc/GMT+9",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Etc/GMT-1",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Etc/GMT-10",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Etc/GMT-11",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Etc/GMT-12",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Etc/GMT-13",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Etc/GMT-14",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Etc/GMT-2",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Etc/GMT-3",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Etc/GMT-4",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Etc/GMT-5",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Etc/GMT-6",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Etc/GMT-7",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Etc/GMT-8",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Etc/GMT-9",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Etc/UTC",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Europe/Amsterdam",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Europe/Andorra",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Europe/Astrakhan",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Europe/Athens",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Europe/Belgrade",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Europe/Berlin",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Europe/Brussels",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Europe/Bucharest",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Europe/Budapest",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Europe/Chisinau",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Europe/Copenhagen",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Europe/Dublin",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Europe/Gibraltar",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Europe/Helsinki",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Europe/Istanbul",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Europe/Kaliningrad",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Europe/Kiev",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Europe/Kirov",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Europe/Lisbon",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Europe/London",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Europe/Luxembourg",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Europe/Madrid",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Europe/Malta",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Europe/Minsk",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Europe/Monaco",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Europe/Moscow",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Europe/Oslo",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Europe/Paris",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Europe/Prague",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Europe/Riga",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Europe/Rome",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Europe/Samara",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Europe/Saratov",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Europe/Simferopol",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Europe/Sofia",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Europe/Stockholm",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Europe/Tallinn",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Europe/Tirane",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Europe/Ulyanovsk",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Europe/Uzhgorod",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Europe/Vienna",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Europe/Vilnius",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Europe/Volgograd",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Europe/Warsaw",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Europe/Zaporozhye",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Europe/Zurich",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Indian/Chagos",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Indian/Christmas",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Indian/Cocos",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Indian/Kerguelen",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Indian/Mahe",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Indian/Maldives",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Indian/Mauritius",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Indian/Reunion",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Pacific/Apia",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Pacific/Auckland",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Pacific/Bougainville",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Pacific/Chatham",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Pacific/Chuuk",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Pacific/Easter",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Pacific/Efate",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Pacific/Enderbury",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Pacific/Fakaofo",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Pacific/Fiji",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Pacific/Funafuti",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Pacific/Galapagos",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Pacific/Gambier",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Pacific/Guadalcanal",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Pacific/Guam",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Pacific/Honolulu",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Pacific/Kiritimati",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Pacific/Kosrae",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Pacific/Kwajalein",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Pacific/Majuro",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Pacific/Marquesas",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Pacific/Nauru",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Pacific/Niue",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Pacific/Norfolk",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Pacific/Noumea",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Pacific/Pago_Pago",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Pacific/Palau",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Pacific/Pitcairn",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Pacific/Pohnpei",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Pacific/Port_Moresby",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Pacific/Rarotonga",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Pacific/Tahiti",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Pacific/Tarawa",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Pacific/Tongatapu",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Pacific/Wake",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Pacific/Wallis",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Root/CET",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Root/CST6CDT",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Root/EET",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Root/EST",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Root/EST5EDT",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Root/HST",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Root/MET",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Root/MST",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Root/MST7MDT",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Root/PST8PDT",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "Root/WET",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        /* [] */0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                              ]
                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                          ]
                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                    ]
                                                                                                                                                                                                                  ]
                                                                                                                                                                                                                ]
                                                                                                                                                                                                              ]
                                                                                                                                                                                                            ]
                                                                                                                                                                                                          ]
                                                                                                                                                                                                        ]
                                                                                                                                                                                                      ]
                                                                                                                                                                                                    ]
                                                                                                                                                                                                  ]
                                                                                                                                                                                                ]
                                                                                                                                                                                              ]
                                                                                                                                                                                            ]
                                                                                                                                                                                          ]
                                                                                                                                                                                        ]
                                                                                                                                                                                      ]
                                                                                                                                                                                    ]
                                                                                                                                                                                  ]
                                                                                                                                                                                ]
                                                                                                                                                                              ]
                                                                                                                                                                            ]
                                                                                                                                                                          ]
                                                                                                                                                                        ]
                                                                                                                                                                      ]
                                                                                                                                                                    ]
                                                                                                                                                                  ]
                                                                                                                                                                ]
                                                                                                                                                              ]
                                                                                                                                                            ]
                                                                                                                                                          ]
                                                                                                                                                        ]
                                                                                                                                                      ]
                                                                                                                                                    ]
                                                                                                                                                  ]
                                                                                                                                                ]
                                                                                                                                              ]
                                                                                                                                            ]
                                                                                                                                          ]
                                                                                                                                        ]
                                                                                                                                      ]
                                                                                                                                    ]
                                                                                                                                  ]
                                                                                                                                ]
                                                                                                                              ]
                                                                                                                            ]
                                                                                                                          ]
                                                                                                                        ]
                                                                                                                      ]
                                                                                                                    ]
                                                                                                                  ]
                                                                                                                ]
                                                                                                              ]
                                                                                                            ]
                                                                                                          ]
                                                                                                        ]
                                                                                                      ]
                                                                                                    ]
                                                                                                  ]
                                                                                                ]
                                                                                              ]
                                                                                            ]
                                                                                          ]
                                                                                        ]
                                                                                      ]
                                                                                    ]
                                                                                  ]
                                                                                ]
                                                                              ]
                                                                            ]
                                                                          ]
                                                                        ]
                                                                      ]
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ]
                                                          ]
                                                        ]
                                                      ]
                                                    ]
                                                  ]
                                                ]
                                              ]
                                            ]
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ];
  /* No side effect */

  function ensure_ge(x, y) {
    if (x >= y) {
      return x;
    } else {
      throw [
            invalid_argument,
            "String.concat"
          ];
    }
  }

  function sum_lengths(_acc, seplen, _param) {
    while(true) {
      var param = _param;
      var acc = _acc;
      if (param) {
        var tl = param[1];
        var hd = param[0];
        if (tl) {
          _param = tl;
          _acc = ensure_ge((hd.length + seplen | 0) + acc | 0, acc);
          continue ;
        } else {
          return hd.length + acc | 0;
        }
      } else {
        return acc;
      }
    }}

  function unsafe_blits(dst, _pos, sep, seplen, _param) {
    while(true) {
      var param = _param;
      var pos = _pos;
      if (param) {
        var tl = param[1];
        var hd = param[0];
        if (tl) {
          caml_blit_string(hd, 0, dst, pos, hd.length);
          caml_blit_string(sep, 0, dst, pos + hd.length | 0, seplen);
          _param = tl;
          _pos = (pos + hd.length | 0) + seplen | 0;
          continue ;
        } else {
          caml_blit_string(hd, 0, dst, pos, hd.length);
          return dst;
        }
      } else {
        return dst;
      }
    }}

  function concat(sep, l) {
    if (l) {
      var seplen = sep.length;
      return bytes_to_string(unsafe_blits(caml_create_bytes(sum_lengths(0, seplen, l)), 0, sep, seplen, l));
    } else {
      return "";
    }
  }

  function equal(prim, prim$1) {
    return prim === prim$1;
  }
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function setStyle(n, key, value) {
    n.style[key] = value;
    return /* () */0;
  }

  function setStyleProperty(n, priorityOpt, key, value) {
    var priority = priorityOpt !== undefined ? priorityOpt : false;
    var style = n.style;
    var match = style.setProperty;
    if (match !== undefined) {
      return style.setProperty(key, value, priority ? "important" : null);
    } else {
      return setStyle(n, key, value);
    }
  }

  function insertBefore(n, child, refNode) {
    return n.insertBefore(child, refNode);
  }

  function setAttributeNsOptional(n, namespace, key, value) {
    if (namespace === "") {
      return n.setAttribute(key, value);
    } else {
      return n.setAttributeNS(namespace, key, value);
    }
  }

  function removeAttributeNsOptional(n, namespace, key) {
    if (namespace === "") {
      return n.removeAttribute(key);
    } else {
      return n.removeAttributeNS(namespace, key);
    }
  }

  function addEventListener(n, typ, listener, options) {
    return n.addEventListener(typ, listener, options);
  }

  function removeEventListener(n, typ, listener, options) {
    return n.removeEventListener(typ, listener, options);
  }

  function remove_polyfill(param) {
    return (// remove polyfill
    (function() {
      if (!('remove' in Element.prototype)) {
        Element.prototype.remove = function() {
          if (this.parentNode) {
            this.parentNode.removeChild(this);
          }
        };
      }  }()));
  }
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function createElementNsOptional(namespace, tagName) {
    if (namespace === "") {
      return document.createElement(tagName);
    } else {
      return document.createElementNS(namespace, tagName);
    }
  }
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  var noNode = /* CommentNode */__(0, [""]);

  function fullnode(namespace, tagName, key, unique, props, vdoms) {
    return /* Node */__(2, [
              namespace,
              tagName,
              key,
              unique,
              props,
              vdoms
            ]);
  }

  function renderToHtmlString(_param) {
    while(true) {
      var param = _param;
      switch (param.tag | 0) {
        case /* CommentNode */0 :
            return "<!-- " + (param[0] + " -->");
        case /* Text */1 :
            return param[0];
        case /* Node */2 :
            var tagName = param[1];
            var namespace = param[0];
            return concat("", /* :: */[
                        "<",
                        /* :: */[
                          namespace,
                          /* :: */[
                            namespace === "" ? "" : ":",
                            /* :: */[
                              tagName,
                              /* :: */[
                                concat("", map((function (p) {
                                            var param = p;
                                            if (typeof param === "number") {
                                              return "";
                                            } else {
                                              switch (param.tag | 0) {
                                                case /* RawProp */0 :
                                                    return concat("", /* :: */[
                                                                " ",
                                                                /* :: */[
                                                                  param[0],
                                                                  /* :: */[
                                                                    "=\"",
                                                                    /* :: */[
                                                                      param[1],
                                                                      /* :: */[
                                                                        "\"",
                                                                        /* [] */0
                                                                      ]
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]);
                                                case /* Attribute */1 :
                                                    return concat("", /* :: */[
                                                                " ",
                                                                /* :: */[
                                                                  param[1],
                                                                  /* :: */[
                                                                    "=\"",
                                                                    /* :: */[
                                                                      param[2],
                                                                      /* :: */[
                                                                        "\"",
                                                                        /* [] */0
                                                                      ]
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]);
                                                case /* Data */2 :
                                                    return concat("", /* :: */[
                                                                " data-",
                                                                /* :: */[
                                                                  param[0],
                                                                  /* :: */[
                                                                    "=\"",
                                                                    /* :: */[
                                                                      param[1],
                                                                      /* :: */[
                                                                        "\"",
                                                                        /* [] */0
                                                                      ]
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]);
                                                case /* Event */3 :
                                                    return "";
                                                case /* Style */4 :
                                                    return concat("", /* :: */[
                                                                " style=\"",
                                                                /* :: */[
                                                                  concat(";", map((function (param) {
                                                                              return concat("", /* :: */[
                                                                                          param[0],
                                                                                          /* :: */[
                                                                                            ":",
                                                                                            /* :: */[
                                                                                              param[1],
                                                                                              /* :: */[
                                                                                                ";",
                                                                                                /* [] */0
                                                                                              ]
                                                                                            ]
                                                                                          ]
                                                                                        ]);
                                                                            }), param[0])),
                                                                  /* :: */[
                                                                    "\"",
                                                                    /* [] */0
                                                                  ]
                                                                ]
                                                              ]);
                                                
                                              }
                                            }
                                          }), param[4])),
                                /* :: */[
                                  ">",
                                  /* :: */[
                                    concat("", map(renderToHtmlString, param[5])),
                                    /* :: */[
                                      "</",
                                      /* :: */[
                                        tagName,
                                        /* :: */[
                                          ">",
                                          /* [] */0
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]);
        case /* LazyGen */3 :
            _param = _1(param[1], /* () */0);
            continue ;
        case /* Tagger */4 :
            _param = param[1];
            continue ;
        
      }
    }}

  function eventHandler(callbacks, cb) {
    return (function (ev) {
        var match = _1(cb.contents, ev);
        if (match !== undefined) {
          return _1(callbacks.contents.enqueue, valFromOption(match));
        } else {
          return /* () */0;
        }
      });
  }

  function eventHandler_GetCB(param) {
    if (param.tag) {
      var msg = param[0];
      return (function (_ev) {
          return some(msg);
        });
    } else {
      return param[1];
    }
  }

  function compareEventHandlerTypes(left, param) {
    if (param.tag) {
      if (left.tag && caml_equal(param[0], left[0])) {
        return true;
      } else {
        return false;
      }
    } else if (left.tag) {
      return false;
    } else {
      return param[0] === left[0];
    }
  }

  function eventHandler_Register(callbacks, elem, name, handlerType) {
    var cb = {
      contents: eventHandler_GetCB(handlerType)
    };
    var handler = eventHandler(callbacks, cb);
    addEventListener(elem, name, handler, false);
    return {
            handler: handler,
            cb: cb
          };
  }

  function eventHandler_Unregister(elem, name, param) {
    if (param !== undefined) {
      removeEventListener(elem, name, param.handler, false);
      return ;
    }
    
  }

  function eventHandler_Mutate(callbacks, elem, oldName, newName, oldHandlerType, newHandlerType, oldCache, newCache) {
    var match = oldCache.contents;
    if (match !== undefined) {
      if (oldName === newName) {
        newCache.contents = oldCache.contents;
        if (compareEventHandlerTypes(oldHandlerType, newHandlerType)) {
          return /* () */0;
        } else {
          var cb = eventHandler_GetCB(newHandlerType);
          match.cb.contents = cb;
          return /* () */0;
        }
      } else {
        oldCache.contents = eventHandler_Unregister(elem, oldName, oldCache.contents);
        newCache.contents = eventHandler_Register(callbacks, elem, newName, newHandlerType);
        return /* () */0;
      }
    } else {
      newCache.contents = eventHandler_Register(callbacks, elem, newName, newHandlerType);
      return /* () */0;
    }
  }

  function patchVNodesOnElems_PropertiesApply_Add(callbacks, elem, _idx, param) {
    if (typeof param === "number") {
      return /* () */0;
    } else {
      switch (param.tag | 0) {
        case /* RawProp */0 :
            elem[param[0]] = param[1];
            return /* () */0;
        case /* Attribute */1 :
            return setAttributeNsOptional(elem, param[0], param[1], param[2]);
        case /* Data */2 :
            console.log(/* tuple */[
                  "TODO:  Add Data Unhandled",
                  param[0],
                  param[1]
                ]);
            throw [
                  failure,
                  "TODO:  Add Data Unhandled"
                ];
        case /* Event */3 :
            param[2].contents = eventHandler_Register(callbacks, elem, param[0], param[1]);
            return /* () */0;
        case /* Style */4 :
            return fold_left((function (param, param$1) {
                          return setStyleProperty(elem, undefined, param$1[0], param$1[1]);
                        }), /* () */0, param[0]);
        
      }
    }
  }

  function patchVNodesOnElems_PropertiesApply_Remove(_callbacks, elem, _idx, param) {
    if (typeof param === "number") {
      return /* () */0;
    } else {
      switch (param.tag | 0) {
        case /* RawProp */0 :
            elem[param[0]] = undefined;
            return /* () */0;
        case /* Attribute */1 :
            return removeAttributeNsOptional(elem, param[0], param[1]);
        case /* Data */2 :
            console.log(/* tuple */[
                  "TODO:  Remove Data Unhandled",
                  param[0],
                  param[1]
                ]);
            throw [
                  failure,
                  "TODO:  Remove Data Unhandled"
                ];
        case /* Event */3 :
            var cache = param[2];
            cache.contents = eventHandler_Unregister(elem, param[0], cache.contents);
            return /* () */0;
        case /* Style */4 :
            return fold_left((function (param, param$1) {
                          return setStyleProperty(elem, undefined, param$1[0], null);
                        }), /* () */0, param[0]);
        
      }
    }
  }

  function patchVNodesOnElems_PropertiesApply_RemoveAdd(callbacks, elem, idx, oldProp, newProp) {
    patchVNodesOnElems_PropertiesApply_Remove(callbacks, elem, idx, oldProp);
    patchVNodesOnElems_PropertiesApply_Add(callbacks, elem, idx, newProp);
    return /* () */0;
  }

  function patchVNodesOnElems_PropertiesApply_Mutate(_callbacks, elem, _idx, oldProp, _newProp) {
    if (typeof _newProp === "number") {
      throw [
            failure,
            "This should never be called as all entries through NoProp are gated."
          ];
    } else {
      switch (_newProp.tag | 0) {
        case /* RawProp */0 :
            elem[_newProp[0]] = _newProp[1];
            return /* () */0;
        case /* Attribute */1 :
            return setAttributeNsOptional(elem, _newProp[0], _newProp[1], _newProp[2]);
        case /* Data */2 :
            console.log(/* tuple */[
                  "TODO:  Mutate Data Unhandled",
                  _newProp[0],
                  _newProp[1]
                ]);
            throw [
                  failure,
                  "TODO:  Mutate Data Unhandled"
                ];
        case /* Event */3 :
            throw [
                  failure,
                  "This will never be called because it is gated"
                ];
        case /* Style */4 :
            if (typeof oldProp === "number") {
              throw [
                    failure,
                    "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!"
                  ];
            } else if (oldProp.tag === /* Style */4) {
              return fold_left2((function (param, param$1, param$2) {
                            var nv = param$2[1];
                            var nk = param$2[0];
                            var ok = param$1[0];
                            if (ok === nk) {
                              if (param$1[1] === nv) {
                                return /* () */0;
                              } else {
                                return setStyleProperty(elem, undefined, nk, nv);
                              }
                            } else {
                              setStyleProperty(elem, undefined, ok, null);
                              return setStyleProperty(elem, undefined, nk, nv);
                            }
                          }), /* () */0, oldProp[0], _newProp[0]);
            } else {
              throw [
                    failure,
                    "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!"
                  ];
            }
        
      }
    }
  }

  function patchVNodesOnElems_PropertiesApply(callbacks, elem, _idx, _oldProperties, _newProperties) {
    while(true) {
      var newProperties = _newProperties;
      var oldProperties = _oldProperties;
      var idx = _idx;
      if (oldProperties) {
        var _oldProp = oldProperties[0];
        if (newProperties) {
          if (typeof _oldProp === "number") {
            if (typeof newProperties[0] === "number") {
              _newProperties = newProperties[1];
              _oldProperties = oldProperties[1];
              _idx = idx + 1 | 0;
              continue ;
            }
            
          } else {
            switch (_oldProp.tag | 0) {
              case /* RawProp */0 :
                  var newProp = newProperties[0];
                  if (typeof newProp !== "number" && !newProp.tag) {
                    if (!(_oldProp[0] === newProp[0] && _oldProp[1] === newProp[1])) {
                      patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, _oldProp, newProp);
                    }
                    _newProperties = newProperties[1];
                    _oldProperties = oldProperties[1];
                    _idx = idx + 1 | 0;
                    continue ;
                  }
                  break;
              case /* Attribute */1 :
                  var newProp$1 = newProperties[0];
                  if (typeof newProp$1 !== "number" && newProp$1.tag === /* Attribute */1) {
                    if (!(_oldProp[0] === newProp$1[0] && _oldProp[1] === newProp$1[1] && _oldProp[2] === newProp$1[2])) {
                      patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, _oldProp, newProp$1);
                    }
                    _newProperties = newProperties[1];
                    _oldProperties = oldProperties[1];
                    _idx = idx + 1 | 0;
                    continue ;
                  }
                  break;
              case /* Data */2 :
                  var newProp$2 = newProperties[0];
                  if (typeof newProp$2 !== "number" && newProp$2.tag === /* Data */2) {
                    if (!(_oldProp[0] === newProp$2[0] && _oldProp[1] === newProp$2[1])) {
                      patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, _oldProp, newProp$2);
                    }
                    _newProperties = newProperties[1];
                    _oldProperties = oldProperties[1];
                    _idx = idx + 1 | 0;
                    continue ;
                  }
                  break;
              case /* Event */3 :
                  var _newProp = newProperties[0];
                  if (typeof _newProp !== "number" && _newProp.tag === /* Event */3) {
                    eventHandler_Mutate(callbacks, elem, _oldProp[0], _newProp[0], _oldProp[1], _newProp[1], _oldProp[2], _newProp[2]);
                    _newProperties = newProperties[1];
                    _oldProperties = oldProperties[1];
                    _idx = idx + 1 | 0;
                    continue ;
                  }
                  break;
              case /* Style */4 :
                  var newProp$3 = newProperties[0];
                  if (typeof newProp$3 !== "number" && newProp$3.tag === /* Style */4) {
                    if (!caml_equal(_oldProp[0], newProp$3[0])) {
                      patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, _oldProp, newProp$3);
                    }
                    _newProperties = newProperties[1];
                    _oldProperties = oldProperties[1];
                    _idx = idx + 1 | 0;
                    continue ;
                  }
                  break;
              
            }
          }
        } else {
          return false;
        }
        patchVNodesOnElems_PropertiesApply_RemoveAdd(callbacks, elem, idx, _oldProp, newProperties[0]);
        _newProperties = newProperties[1];
        _oldProperties = oldProperties[1];
        _idx = idx + 1 | 0;
        continue ;
      } else if (newProperties) {
        return false;
      } else {
        return true;
      }
    }}

  function patchVNodesOnElems_Properties(callbacks, elem, oldProperties, newProperties) {
    return patchVNodesOnElems_PropertiesApply(callbacks, elem, 0, oldProperties, newProperties);
  }

  function patchVNodesOnElems_ReplaceNode(callbacks, elem, elems, idx, param) {
    if (param.tag === /* Node */2) {
      var newProperties = param[4];
      var oldChild = caml_array_get(elems, idx);
      var newChild = createElementNsOptional(param[0], param[1]);
      var match = patchVNodesOnElems_Properties(callbacks, newChild, map((function (param) {
                  return /* NoProp */0;
                }), newProperties), newProperties);
      if (match) {
        var childChildren = newChild.childNodes;
        patchVNodesOnElems(callbacks, newChild, childChildren, 0, /* [] */0, param[5]);
        insertBefore(elem, newChild, oldChild);
        elem.removeChild(oldChild);
        return /* () */0;
      } else {
        throw [
              match_failure,
              /* tuple */[
                "vdom.ml",
                343,
                13
              ]
            ];
      }
    } else {
      throw [
            failure,
            "Node replacement should never be passed anything but a node itself"
          ];
    }
  }

  function patchVNodesOnElems_CreateElement(_callbacks, _param) {
    while(true) {
      var param = _param;
      var callbacks = _callbacks;
      switch (param.tag | 0) {
        case /* CommentNode */0 :
            var text = param[0];
            return document.createComment(text);
        case /* Text */1 :
            var text$1 = param[0];
            return document.createTextNode(text$1);
        case /* Node */2 :
            var newProperties = param[4];
            var newChild = createElementNsOptional(param[0], param[1]);
            var match = patchVNodesOnElems_Properties(callbacks, newChild, map((function (param) {
                        return /* NoProp */0;
                      }), newProperties), newProperties);
            if (match) {
              var childChildren = newChild.childNodes;
              patchVNodesOnElems(callbacks, newChild, childChildren, 0, /* [] */0, param[5]);
              return newChild;
            } else {
              throw [
                    match_failure,
                    /* tuple */[
                      "vdom.ml",
                      368,
                      11
                    ]
                  ];
            }
        case /* LazyGen */3 :
            var vdom = _1(param[1], /* () */0);
            param[2].contents = vdom;
            _param = vdom;
            continue ;
        case /* Tagger */4 :
            _param = param[1];
            _callbacks = _1(param[0], callbacks);
            continue ;
        
      }
    }}

  function patchVNodesOnElems_MutateNode(callbacks, elem, elems, idx, oldNode, newNode) {
    if (oldNode.tag === /* Node */2) {
      if (newNode.tag === /* Node */2) {
        if (oldNode[3] !== newNode[3] || oldNode[1] !== newNode[1]) {
          return patchVNodesOnElems_ReplaceNode(callbacks, elem, elems, idx, newNode);
        } else {
          var child = caml_array_get(elems, idx);
          var childChildren = child.childNodes;
          if (!patchVNodesOnElems_Properties(callbacks, child, oldNode[4], newNode[4])) {
            console.log("VDom:  Failed swapping properties because the property list length changed, use `noProp` to swap properties instead, not by altering the list structure.  This is a massive inefficiency until this issue is resolved.");
            patchVNodesOnElems_ReplaceNode(callbacks, elem, elems, idx, newNode);
          }
          return patchVNodesOnElems(callbacks, child, childChildren, 0, oldNode[5], newNode[5]);
        }
      } else {
        throw [
              failure,
              "Non-node passed to patchVNodesOnElems_MutateNode"
            ];
      }
    } else {
      throw [
            failure,
            "Non-node passed to patchVNodesOnElems_MutateNode"
          ];
    }
  }

  function patchVNodesOnElems(callbacks, elem, elems, _idx, _oldVNodes, _newVNodes) {
    while(true) {
      var newVNodes = _newVNodes;
      var oldVNodes = _oldVNodes;
      var idx = _idx;
      if (oldVNodes) {
        var oldNode = oldVNodes[0];
        switch (oldNode.tag | 0) {
          case /* CommentNode */0 :
              if (newVNodes) {
                var match = newVNodes[0];
                if (!match.tag && oldNode[0] === match[0]) {
                  _newVNodes = newVNodes[1];
                  _oldVNodes = oldVNodes[1];
                  _idx = idx + 1 | 0;
                  continue ;
                }
                
              }
              break;
          case /* Text */1 :
              if (newVNodes) {
                var match$1 = newVNodes[0];
                if (match$1.tag === /* Text */1) {
                  var newText = match$1[0];
                  if (oldNode[0] !== newText) {
                    var child = caml_array_get(elems, idx);
                    child.nodeValue = newText;
                  }
                  _newVNodes = newVNodes[1];
                  _oldVNodes = oldVNodes[1];
                  _idx = idx + 1 | 0;
                  continue ;
                }
                
              }
              break;
          case /* Node */2 :
              if (newVNodes) {
                var newNode = newVNodes[0];
                if (newNode.tag === /* Node */2) {
                  var newRest = newVNodes[1];
                  var newKey = newNode[2];
                  var newTagName = newNode[1];
                  var newNamespace = newNode[0];
                  var oldRest = oldVNodes[1];
                  var oldKey = oldNode[2];
                  var oldTagName = oldNode[1];
                  var oldNamespace = oldNode[0];
                  if (oldKey === newKey && oldKey !== "") {
                    _newVNodes = newRest;
                    _oldVNodes = oldRest;
                    _idx = idx + 1 | 0;
                    continue ;
                  } else if (oldKey === "" || newKey === "") {
                    patchVNodesOnElems_MutateNode(callbacks, elem, elems, idx, oldNode, newNode);
                    _newVNodes = newRest;
                    _oldVNodes = oldRest;
                    _idx = idx + 1 | 0;
                    continue ;
                  } else {
                    var exit = 0;
                    var exit$1 = 0;
                    if (oldRest) {
                      var match$2 = oldRest[0];
                      if (match$2.tag === /* Node */2) {
                        var olderRest = oldRest[1];
                        var olderKey = match$2[2];
                        var olderTagName = match$2[1];
                        var olderNamespace = match$2[0];
                        var exit$2 = 0;
                        if (newRest) {
                          var match$3 = newRest[0];
                          if (match$3.tag === /* Node */2 && olderNamespace === newNamespace && olderTagName === newTagName && olderKey === newKey && oldNamespace === match$3[0] && oldTagName === match$3[1] && oldKey === match$3[2]) {
                            var firstChild = caml_array_get(elems, idx);
                            var secondChild = caml_array_get(elems, idx + 1 | 0);
                            elem.removeChild(secondChild);
                            insertBefore(elem, secondChild, firstChild);
                            _newVNodes = newRest[1];
                            _oldVNodes = olderRest;
                            _idx = idx + 2 | 0;
                            continue ;
                          } else {
                            exit$2 = 4;
                          }
                        } else {
                          exit$2 = 4;
                        }
                        if (exit$2 === 4) {
                          if (olderNamespace === newNamespace && olderTagName === newTagName && olderKey === newKey) {
                            var oldChild = caml_array_get(elems, idx);
                            elem.removeChild(oldChild);
                            _newVNodes = newRest;
                            _oldVNodes = olderRest;
                            _idx = idx + 1 | 0;
                            continue ;
                          } else {
                            exit$1 = 3;
                          }
                        }
                        
                      } else {
                        exit$1 = 3;
                      }
                    } else {
                      exit$1 = 3;
                    }
                    if (exit$1 === 3) {
                      if (newRest) {
                        var match$4 = newRest[0];
                        if (match$4.tag === /* Node */2 && oldNamespace === match$4[0] && oldTagName === match$4[1] && oldKey === match$4[2]) {
                          var oldChild$1 = caml_array_get(elems, idx);
                          var newChild = patchVNodesOnElems_CreateElement(callbacks, newNode);
                          insertBefore(elem, newChild, oldChild$1);
                          _newVNodes = newRest;
                          _idx = idx + 1 | 0;
                          continue ;
                        } else {
                          exit = 2;
                        }
                      } else {
                        exit = 2;
                      }
                    }
                    if (exit === 2) {
                      patchVNodesOnElems_MutateNode(callbacks, elem, elems, idx, oldNode, newNode);
                      _newVNodes = newRest;
                      _oldVNodes = oldRest;
                      _idx = idx + 1 | 0;
                      continue ;
                    }
                    
                  }
                }
                
              }
              break;
          case /* LazyGen */3 :
              if (newVNodes) {
                var match$5 = newVNodes[0];
                if (match$5.tag === /* LazyGen */3) {
                  var newRest$1 = newVNodes[1];
                  var newCache = match$5[2];
                  var newGen = match$5[1];
                  var newKey$1 = match$5[0];
                  var oldRest$1 = oldVNodes[1];
                  var oldCache = oldNode[2];
                  var oldKey$1 = oldNode[0];
                  if (oldKey$1 === newKey$1) {
                    newCache.contents = oldCache.contents;
                    _newVNodes = newRest$1;
                    _oldVNodes = oldRest$1;
                    _idx = idx + 1 | 0;
                    continue ;
                  } else {
                    var exit$3 = 0;
                    var exit$4 = 0;
                    if (oldRest$1) {
                      var match$6 = oldRest$1[0];
                      if (match$6.tag === /* LazyGen */3) {
                        var olderRest$1 = oldRest$1[1];
                        var olderKey$1 = match$6[0];
                        var exit$5 = 0;
                        if (newRest$1) {
                          var match$7 = newRest$1[0];
                          if (match$7.tag === /* LazyGen */3 && olderKey$1 === newKey$1 && oldKey$1 === match$7[0]) {
                            var firstChild$1 = caml_array_get(elems, idx);
                            var secondChild$1 = caml_array_get(elems, idx + 1 | 0);
                            elem.removeChild(secondChild$1);
                            insertBefore(elem, secondChild$1, firstChild$1);
                            _newVNodes = newRest$1[1];
                            _oldVNodes = olderRest$1;
                            _idx = idx + 2 | 0;
                            continue ;
                          } else {
                            exit$5 = 4;
                          }
                        } else {
                          exit$5 = 4;
                        }
                        if (exit$5 === 4) {
                          if (olderKey$1 === newKey$1) {
                            var oldChild$2 = caml_array_get(elems, idx);
                            elem.removeChild(oldChild$2);
                            var oldVdom = match$6[2].contents;
                            newCache.contents = oldVdom;
                            _newVNodes = newRest$1;
                            _oldVNodes = olderRest$1;
                            _idx = idx + 1 | 0;
                            continue ;
                          } else {
                            exit$4 = 3;
                          }
                        }
                        
                      } else {
                        exit$4 = 3;
                      }
                    } else {
                      exit$4 = 3;
                    }
                    if (exit$4 === 3) {
                      if (newRest$1) {
                        var match$8 = newRest$1[0];
                        if (match$8.tag === /* LazyGen */3 && match$8[0] === oldKey$1) {
                          var oldChild$3 = caml_array_get(elems, idx);
                          var newVdom = _1(newGen, /* () */0);
                          newCache.contents = newVdom;
                          var newChild$1 = patchVNodesOnElems_CreateElement(callbacks, newVdom);
                          insertBefore(elem, newChild$1, oldChild$3);
                          _newVNodes = newRest$1;
                          _idx = idx + 1 | 0;
                          continue ;
                        } else {
                          exit$3 = 2;
                        }
                      } else {
                        exit$3 = 2;
                      }
                    }
                    if (exit$3 === 2) {
                      var oldVdom$1 = oldCache.contents;
                      var newVdom$1 = _1(newGen, /* () */0);
                      newCache.contents = newVdom$1;
                      _newVNodes = /* :: */[
                        newVdom$1,
                        newRest$1
                      ];
                      _oldVNodes = /* :: */[
                        oldVdom$1,
                        oldRest$1
                      ];
                      continue ;
                    }
                    
                  }
                }
                
              }
              break;
          case /* Tagger */4 :
              _oldVNodes = /* :: */[
                oldNode[1],
                oldVNodes[1]
              ];
              continue ;
          
        }
        var oldRest$2 = oldVNodes[1];
        if (newVNodes) {
          var newNode$1 = newVNodes[0];
          if (newNode$1.tag === /* Tagger */4) {
            patchVNodesOnElems(_1(newNode$1[0], callbacks), elem, elems, idx, /* :: */[
                  oldNode,
                  /* [] */0
                ], /* :: */[
                  newNode$1[1],
                  /* [] */0
                ]);
            _newVNodes = newVNodes[1];
            _oldVNodes = oldRest$2;
            _idx = idx + 1 | 0;
            continue ;
          } else {
            var oldChild$4 = caml_array_get(elems, idx);
            var newChild$2 = patchVNodesOnElems_CreateElement(callbacks, newNode$1);
            insertBefore(elem, newChild$2, oldChild$4);
            elem.removeChild(oldChild$4);
            _newVNodes = newVNodes[1];
            _oldVNodes = oldRest$2;
            _idx = idx + 1 | 0;
            continue ;
          }
        } else {
          var child$1 = caml_array_get(elems, idx);
          elem.removeChild(child$1);
          _newVNodes = /* [] */0;
          _oldVNodes = oldRest$2;
          continue ;
        }
      } else if (newVNodes) {
        var newChild$3 = patchVNodesOnElems_CreateElement(callbacks, newVNodes[0]);
        elem.appendChild(newChild$3);
        _newVNodes = newVNodes[1];
        _oldVNodes = /* [] */0;
        _idx = idx + 1 | 0;
        continue ;
      } else {
        return /* () */0;
      }
    }}

  function patchVNodesIntoElement(callbacks, elem, oldVNodes, newVNodes) {
    var elems = elem.childNodes;
    patchVNodesOnElems(callbacks, elem, elems, 0, oldVNodes, newVNodes);
    return newVNodes;
  }
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function requestAnimationFrame_polyfill(param) {
    return (// requestAnimationFrame polyfill
    (function() {
        var lastTime = 0;
        var vendors = ['ms', 'moz', 'webkit', 'o'];
        for(var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
            window.requestAnimationFrame = window[vendors[x]+'RequestAnimationFrame'];
            window.cancelAnimationFrame = window[vendors[x]+'CancelAnimationFrame']
                                       || window[vendors[x]+'CancelRequestAnimationFrame'];
        }

        if (!window.requestAnimationFrame)
            window.requestAnimationFrame = function(callback, element) {
                var currTime = new Date().getTime();
                var timeToCall = Math.max(0, 16 - (currTime - lastTime));
                var id = window.setTimeout(function() { callback(currTime + timeToCall); },
                  timeToCall);
                lastTime = currTime + timeToCall;
                return id;
            };

        if (!window.cancelAnimationFrame)
            window.cancelAnimationFrame = function(id) {
                clearTimeout(id);
            };
    }()));
  }
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function polyfills(param) {
    remove_polyfill();
    requestAnimationFrame_polyfill();
    return /* () */0;
  }
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function run(_callbacks, _param) {
    while(true) {
      var param = _param;
      var callbacks = _callbacks;
      if (typeof param === "number") {
        return /* () */0;
      } else {
        switch (param.tag | 0) {
          case /* Mapper */0 :
              var subCallbacks = _1(param[0], callbacks);
              _param = param[1];
              _callbacks = subCallbacks;
              continue ;
          case /* Batch */1 :
              return fold_left((function(callbacks){
                        return function (param, cmd) {
                          return run(callbacks, cmd);
                        }
                        }(callbacks)), /* () */0, param[0]);
          case /* EnqueueCall */2 :
              return _1(param[0], callbacks);
          
        }
      }
    }}
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function run$1(oldCallbacks, newCallbacks, oldSub, newSub) {
    var enable = function (_callbacks, _param) {
      while(true) {
        var param = _param;
        var callbacks = _callbacks;
        if (typeof param === "number") {
          return /* () */0;
        } else {
          switch (param.tag | 0) {
            case /* Batch */0 :
                var subs = param[0];
                if (subs) {
                  return iter((function(callbacks){
                            return function (param) {
                              return enable(callbacks, param);
                            }
                            }(callbacks)), subs);
                } else {
                  return /* () */0;
                }
            case /* Registration */1 :
                param[2].contents = _1(param[1], callbacks);
                return /* () */0;
            case /* Mapper */2 :
                var subCallbacks = _1(param[0], callbacks);
                _param = param[1];
                _callbacks = subCallbacks;
                continue ;
            
          }
        }
      }  };
    var disable = function (_callbacks, _param) {
      while(true) {
        var param = _param;
        var callbacks = _callbacks;
        if (typeof param === "number") {
          return /* () */0;
        } else {
          switch (param.tag | 0) {
            case /* Batch */0 :
                var subs = param[0];
                if (subs) {
                  return iter((function(callbacks){
                            return function (param) {
                              return disable(callbacks, param);
                            }
                            }(callbacks)), subs);
                } else {
                  return /* () */0;
                }
            case /* Registration */1 :
                var diCB = param[2];
                var match = diCB.contents;
                if (match !== undefined) {
                  diCB.contents = undefined;
                  return _1(match, /* () */0);
                } else {
                  return /* () */0;
                }
            case /* Mapper */2 :
                var subCallbacks = _1(param[0], callbacks);
                _param = param[1];
                _callbacks = subCallbacks;
                continue ;
            
          }
        }
      }  };
    if (typeof oldSub === "number") {
      if (typeof newSub === "number") {
        return newSub;
      }
      
    } else {
      switch (oldSub.tag | 0) {
        case /* Batch */0 :
            if (typeof newSub !== "number" && !newSub.tag) {
              var aux = function (_oldList, _newList) {
                while(true) {
                  var newList = _newList;
                  var oldList = _oldList;
                  if (oldList) {
                    var oldRest = oldList[1];
                    var oldSubSub = oldList[0];
                    if (newList) {
                      run$1(oldCallbacks, newCallbacks, oldSubSub, newList[0]);
                      _newList = newList[1];
                      _oldList = oldRest;
                      continue ;
                    } else {
                      disable(oldCallbacks, oldSubSub);
                      _newList = /* [] */0;
                      _oldList = oldRest;
                      continue ;
                    }
                  } else if (newList) {
                    enable(newCallbacks, newList[0]);
                    _newList = newList[1];
                    _oldList = /* [] */0;
                    continue ;
                  } else {
                    return /* () */0;
                  }
                }            };
              aux(oldSub[0], newSub[0]);
              return newSub;
            }
            break;
        case /* Registration */1 :
            if (typeof newSub !== "number" && newSub.tag === /* Registration */1 && oldSub[0] === newSub[0]) {
              newSub[2].contents = oldSub[2].contents;
              return newSub;
            }
            break;
        case /* Mapper */2 :
            if (typeof newSub !== "number" && newSub.tag === /* Mapper */2) {
              var olderCallbacks = _1(oldSub[0], oldCallbacks);
              var newerCallbacks = _1(newSub[0], newCallbacks);
              run$1(olderCallbacks, newerCallbacks, oldSub[1], newSub[1]);
              return newSub;
            }
            break;
        
      }
    }
    disable(oldCallbacks, oldSub);
    enable(newCallbacks, newSub);
    return newSub;
  }
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function programStateWrapper(initModel, pump, shutdown) {
    var model = {
      contents: initModel
    };
    var callbacks = {
      contents: {
        enqueue: (function (_msg) {
            console.log("INVALID enqueue CALL!");
            return /* () */0;
          }),
        on: (function (param) {
            return /* () */0;
          })
      }
    };
    var pumperInterface = _1(pump, callbacks);
    var pending = {
      contents: undefined
    };
    var handler = function (msg) {
      var match = pending.contents;
      if (match !== undefined) {
        pending.contents = /* :: */[
          msg,
          match
        ];
        return /* () */0;
      } else {
        pending.contents = /* [] */0;
        var newModel = _2(pumperInterface.handleMsg, model.contents, msg);
        model.contents = newModel;
        var match$1 = pending.contents;
        if (match$1 !== undefined) {
          var msgs = match$1;
          if (msgs) {
            pending.contents = undefined;
            return iter(handler, rev(msgs));
          } else {
            pending.contents = undefined;
            return /* () */0;
          }
        } else {
          throw [
                failure,
                "INVALID message queue state, should never be None during message processing!"
              ];
        }
      }
    };
    var render_events = {
      contents: /* [] */0
    };
    var finalizedCBs_enqueue = handler;
    var finalizedCBs_on = function (param) {
      if (typeof param === "number") {
        return iter(handler, render_events.contents);
      } else if (param.tag) {
        var msg = param[0];
        render_events.contents = filter((function (mg) {
                  return msg !== mg;
                }))(render_events.contents);
        return /* () */0;
      } else {
        render_events.contents = append(render_events.contents, /* :: */[
              param[0],
              /* [] */0
            ]);
        return /* () */0;
      }
    };
    var finalizedCBs = {
      enqueue: finalizedCBs_enqueue,
      on: finalizedCBs_on
    };
    callbacks.contents = finalizedCBs;
    var pi_requestShutdown = function (param) {
      callbacks.contents = {
        enqueue: (function (_msg) {
            console.log("INVALID message enqueued when shut down");
            return /* () */0;
          }),
        on: (function (param) {
            return /* () */0;
          })
      };
      var cmd = _1(shutdown, model.contents);
      _1(pumperInterface.shutdown, cmd);
      return /* () */0;
    };
    var render_string = function (param) {
      return _1(pumperInterface.render_string, model.contents);
    };
    _1(pumperInterface.startup, /* () */0);
    return {
            pushMsg: handler,
            shutdown: pi_requestShutdown,
            getHtmlString: render_string
          };
  }

  function programLoop(update, view, subscriptions, initModel, initCmd, param) {
    if (param !== undefined) {
      var parentNode = valFromOption(param);
      return (function (callbacks) {
          var priorRenderedVdom = {
            contents: /* [] */0
          };
          var latestModel = {
            contents: initModel
          };
          var nextFrameID = {
            contents: undefined
          };
          var doRender = function (_delta) {
            var match = nextFrameID.contents;
            if (match !== undefined) {
              var newVdom_000 = _1(view, latestModel.contents);
              var newVdom = /* :: */[
                newVdom_000,
                /* [] */0
              ];
              var justRenderedVdom = patchVNodesIntoElement(callbacks, parentNode, priorRenderedVdom.contents, newVdom);
              priorRenderedVdom.contents = justRenderedVdom;
              _1(callbacks.contents.on, /* Render */0);
              nextFrameID.contents = undefined;
              return /* () */0;
            } else {
              return /* () */0;
            }
          };
          var scheduleRender = function (param) {
            var match = nextFrameID.contents;
            if (match !== undefined) {
              return /* () */0;
            } else {
              var id = window.requestAnimationFrame(doRender);
              nextFrameID.contents = id;
              return /* () */0;
            }
          };
          var clearPnode = function (param) {
            while(parentNode.childNodes.length > 0) {
              var match = parentNode.firstChild;
              if (match !== null) {
                parentNode.removeChild(match);
              }
              
            }          return /* () */0;
          };
          var oldSub = {
            contents: /* NoSub */0
          };
          var handleSubscriptionChange = function (model) {
            var newSub = _1(subscriptions, model);
            oldSub.contents = run$1(callbacks, callbacks, oldSub.contents, newSub);
            return /* () */0;
          };
          var handlerStartup = function (param) {
            clearPnode();
            run(callbacks, initCmd);
            handleSubscriptionChange(latestModel.contents);
            nextFrameID.contents = -1;
            doRender();
            return /* () */0;
          };
          var render_string = function (model) {
            return renderToHtmlString(_1(view, model));
          };
          var handler = function (model, msg) {
            var match = _2(update, model, msg);
            var newModel = match[0];
            latestModel.contents = newModel;
            run(callbacks, match[1]);
            scheduleRender();
            handleSubscriptionChange(newModel);
            return newModel;
          };
          var handlerShutdown = function (cmd) {
            nextFrameID.contents = undefined;
            run(callbacks, cmd);
            oldSub.contents = run$1(callbacks, callbacks, oldSub.contents, /* NoSub */0);
            priorRenderedVdom.contents = /* [] */0;
            clearPnode();
            return /* () */0;
          };
          return {
                  startup: handlerStartup,
                  render_string: render_string,
                  handleMsg: handler,
                  shutdown: handlerShutdown
                };
        });
    } else {
      return (function (callbacks) {
          var oldSub = {
            contents: /* NoSub */0
          };
          var handleSubscriptionChange = function (model) {
            var newSub = _1(subscriptions, model);
            oldSub.contents = run$1(callbacks, callbacks, oldSub.contents, newSub);
            return /* () */0;
          };
          return {
                  startup: (function (param) {
                      run(callbacks, initCmd);
                      handleSubscriptionChange(initModel);
                      return /* () */0;
                    }),
                  render_string: (function (model) {
                      return renderToHtmlString(_1(view, model));
                    }),
                  handleMsg: (function (model, msg) {
                      var match = _2(update, model, msg);
                      var newModel = match[0];
                      run(callbacks, match[1]);
                      handleSubscriptionChange(newModel);
                      return newModel;
                    }),
                  shutdown: (function (cmd) {
                      run(callbacks, cmd);
                      oldSub.contents = run$1(callbacks, callbacks, oldSub.contents, /* NoSub */0);
                      return /* () */0;
                    })
                };
        });
    }
  }

  function program(param, pnode, flags) {
    polyfills();
    var match = _1(param.init, flags);
    var initModel = match[0];
    var opnode = (pnode == null) ? undefined : some(pnode);
    var pumpInterface = programLoop(param.update, param.view, param.subscriptions, initModel, match[1], opnode);
    return programStateWrapper(initModel, pumpInterface, param.shutdown);
  }

  function standardProgram(param, pnode, args) {
    return program({
                init: param.init,
                update: param.update,
                view: param.view,
                subscriptions: param.subscriptions,
                shutdown: (function (_model) {
                    return /* NoCmd */0;
                  })
              }, pnode, args);
  }

  function beginnerProgram(param, pnode, param$1) {
    var update = param.update;
    var model = param.model;
    return standardProgram({
                init: (function (param) {
                    return /* tuple */[
                            model,
                            /* NoCmd */0
                          ];
                  }),
                update: (function (model, msg) {
                    return /* tuple */[
                            _2(update, model, msg),
                            /* NoCmd */0
                          ];
                  }),
                view: param.view,
                subscriptions: (function (_model) {
                    return /* NoSub */0;
                  })
              }, pnode, /* () */0);
  }
  /* No side effect */

  function map$1(f, a) {
    var l = a.length;
    if (l === 0) {
      return [];
    } else {
      var r = caml_make_vect(l, _1(f, a[0]));
      for(var i = 1 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
        r[i] = _1(f, a[i]);
      }
      return r;
    }
  }

  function to_list(a) {
    var _i = a.length - 1 | 0;
    var _res = /* [] */0;
    while(true) {
      var res = _res;
      var i = _i;
      if (i < 0) {
        return res;
      } else {
        _res = /* :: */[
          a[i],
          res
        ];
        _i = i - 1 | 0;
        continue ;
      }
    }}

  function fold_right$1(f, a, x) {
    var r = x;
    for(var i = a.length - 1 | 0; i >= 0; --i){
      r = _2(f, a[i], r);
    }
    return r;
  }

  var Bottom = create("Array.Bottom");
  /* No side effect */

  function get(dict, k) {
    if ((k in dict)) {
      return some(dict[k]);
    }
    
  }
  /* No side effect */

  function classify(x) {
    var ty = typeof x;
    if (ty === "string") {
      return /* JSONString */__(0, [x]);
    } else if (ty === "number") {
      return /* JSONNumber */__(1, [x]);
    } else if (ty === "boolean") {
      if (x === true) {
        return /* JSONTrue */1;
      } else {
        return /* JSONFalse */0;
      }
    } else if (x === null) {
      return /* JSONNull */2;
    } else if (Array.isArray(x)) {
      return /* JSONArray */__(3, [x]);
    } else {
      return /* JSONObject */__(2, [x]);
    }
  }
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  var classify$1 = classify;
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function error(param) {
    if (param.tag) {
      return some(param[0]);
    }
    
  }

  function first(fst, e) {
    if (e.tag) {
      return e;
    } else {
      return fst;
    }
  }

  function error_of_first(fst, param) {
    if (param.tag) {
      return some(param[0]);
    } else {
      return error(fst);
    }
  }
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function height(param) {
    if (param) {
      return param[/* h */4];
    } else {
      return 0;
    }
  }

  function create$1(l, x, d, r) {
    var hl = height(l);
    var hr = height(r);
    return /* Node */[
            /* l */l,
            /* v */x,
            /* d */d,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }

  function singleton(x, d) {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */d,
            /* r : Empty */0,
            /* h */1
          ];
  }

  function bal(l, x, d, r) {
    var hl = l ? l[/* h */4] : 0;
    var hr = r ? r[/* h */4] : 0;
    if (hl > (hr + 2 | 0)) {
      if (l) {
        var lr = l[/* r */3];
        var ld = l[/* d */2];
        var lv = l[/* v */1];
        var ll = l[/* l */0];
        if (height(ll) >= height(lr)) {
          return create$1(ll, lv, ld, create$1(lr, x, d, r));
        } else if (lr) {
          return create$1(create$1(ll, lv, ld, lr[/* l */0]), lr[/* v */1], lr[/* d */2], create$1(lr[/* r */3], x, d, r));
        } else {
          throw [
                invalid_argument,
                "Map.bal"
              ];
        }
      } else {
        throw [
              invalid_argument,
              "Map.bal"
            ];
      }
    } else if (hr > (hl + 2 | 0)) {
      if (r) {
        var rr = r[/* r */3];
        var rd = r[/* d */2];
        var rv = r[/* v */1];
        var rl = r[/* l */0];
        if (height(rr) >= height(rl)) {
          return create$1(create$1(l, x, d, rl), rv, rd, rr);
        } else if (rl) {
          return create$1(create$1(l, x, d, rl[/* l */0]), rl[/* v */1], rl[/* d */2], create$1(rl[/* r */3], rv, rd, rr));
        } else {
          throw [
                invalid_argument,
                "Map.bal"
              ];
        }
      } else {
        throw [
              invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      return /* Node */[
              /* l */l,
              /* v */x,
              /* d */d,
              /* r */r,
              /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
            ];
    }
  }

  function is_empty(param) {
    if (param) {
      return false;
    } else {
      return true;
    }
  }

  function add(x, data, m) {
    if (m) {
      var r = m[/* r */3];
      var d = m[/* d */2];
      var v = m[/* v */1];
      var l = m[/* l */0];
      var c = caml_string_compare(x, v);
      if (c === 0) {
        if (d === data) {
          return m;
        } else {
          return /* Node */[
                  /* l */l,
                  /* v */x,
                  /* d */data,
                  /* r */r,
                  /* h */m[/* h */4]
                ];
        }
      } else if (c < 0) {
        var ll = add(x, data, l);
        if (l === ll) {
          return m;
        } else {
          return bal(ll, v, d, r);
        }
      } else {
        var rr = add(x, data, r);
        if (r === rr) {
          return m;
        } else {
          return bal(l, v, d, rr);
        }
      }
    } else {
      return /* Node */[
              /* l : Empty */0,
              /* v */x,
              /* d */data,
              /* r : Empty */0,
              /* h */1
            ];
    }
  }

  function find(x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var c = caml_string_compare(x, param[/* v */1]);
        if (c === 0) {
          return param[/* d */2];
        } else {
          _param = c < 0 ? param[/* l */0] : param[/* r */3];
          continue ;
        }
      } else {
        throw not_found;
      }
    }}

  function find_first(f, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var v = param[/* v */1];
        if (_1(f, v)) {
          var _v0 = v;
          var _d0 = param[/* d */2];
          var f$1 = f;
          var _param$1 = param[/* l */0];
          while(true) {
            var param$1 = _param$1;
            var d0 = _d0;
            var v0 = _v0;
            if (param$1) {
              var v$1 = param$1[/* v */1];
              if (_1(f$1, v$1)) {
                _param$1 = param$1[/* l */0];
                _d0 = param$1[/* d */2];
                _v0 = v$1;
                continue ;
              } else {
                _param$1 = param$1[/* r */3];
                continue ;
              }
            } else {
              return /* tuple */[
                      v0,
                      d0
                    ];
            }
          }      } else {
          _param = param[/* r */3];
          continue ;
        }
      } else {
        throw not_found;
      }
    }}

  function find_first_opt(f, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var v = param[/* v */1];
        if (_1(f, v)) {
          var _v0 = v;
          var _d0 = param[/* d */2];
          var f$1 = f;
          var _param$1 = param[/* l */0];
          while(true) {
            var param$1 = _param$1;
            var d0 = _d0;
            var v0 = _v0;
            if (param$1) {
              var v$1 = param$1[/* v */1];
              if (_1(f$1, v$1)) {
                _param$1 = param$1[/* l */0];
                _d0 = param$1[/* d */2];
                _v0 = v$1;
                continue ;
              } else {
                _param$1 = param$1[/* r */3];
                continue ;
              }
            } else {
              return /* tuple */[
                      v0,
                      d0
                    ];
            }
          }      } else {
          _param = param[/* r */3];
          continue ;
        }
      } else {
        return ;
      }
    }}

  function find_last(f, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var v = param[/* v */1];
        if (_1(f, v)) {
          var _v0 = v;
          var _d0 = param[/* d */2];
          var f$1 = f;
          var _param$1 = param[/* r */3];
          while(true) {
            var param$1 = _param$1;
            var d0 = _d0;
            var v0 = _v0;
            if (param$1) {
              var v$1 = param$1[/* v */1];
              if (_1(f$1, v$1)) {
                _param$1 = param$1[/* r */3];
                _d0 = param$1[/* d */2];
                _v0 = v$1;
                continue ;
              } else {
                _param$1 = param$1[/* l */0];
                continue ;
              }
            } else {
              return /* tuple */[
                      v0,
                      d0
                    ];
            }
          }      } else {
          _param = param[/* l */0];
          continue ;
        }
      } else {
        throw not_found;
      }
    }}

  function find_last_opt(f, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var v = param[/* v */1];
        if (_1(f, v)) {
          var _v0 = v;
          var _d0 = param[/* d */2];
          var f$1 = f;
          var _param$1 = param[/* r */3];
          while(true) {
            var param$1 = _param$1;
            var d0 = _d0;
            var v0 = _v0;
            if (param$1) {
              var v$1 = param$1[/* v */1];
              if (_1(f$1, v$1)) {
                _param$1 = param$1[/* r */3];
                _d0 = param$1[/* d */2];
                _v0 = v$1;
                continue ;
              } else {
                _param$1 = param$1[/* l */0];
                continue ;
              }
            } else {
              return /* tuple */[
                      v0,
                      d0
                    ];
            }
          }      } else {
          _param = param[/* l */0];
          continue ;
        }
      } else {
        return ;
      }
    }}

  function find_opt(x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var c = caml_string_compare(x, param[/* v */1]);
        if (c === 0) {
          return some(param[/* d */2]);
        } else {
          _param = c < 0 ? param[/* l */0] : param[/* r */3];
          continue ;
        }
      } else {
        return ;
      }
    }}

  function mem(x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var c = caml_string_compare(x, param[/* v */1]);
        if (c === 0) {
          return true;
        } else {
          _param = c < 0 ? param[/* l */0] : param[/* r */3];
          continue ;
        }
      } else {
        return false;
      }
    }}

  function min_binding(_param) {
    while(true) {
      var param = _param;
      if (param) {
        var l = param[/* l */0];
        if (l) {
          _param = l;
          continue ;
        } else {
          return /* tuple */[
                  param[/* v */1],
                  param[/* d */2]
                ];
        }
      } else {
        throw not_found;
      }
    }}

  function min_binding_opt(_param) {
    while(true) {
      var param = _param;
      if (param) {
        var l = param[/* l */0];
        if (l) {
          _param = l;
          continue ;
        } else {
          return /* tuple */[
                  param[/* v */1],
                  param[/* d */2]
                ];
        }
      } else {
        return ;
      }
    }}

  function max_binding(_param) {
    while(true) {
      var param = _param;
      if (param) {
        var r = param[/* r */3];
        if (r) {
          _param = r;
          continue ;
        } else {
          return /* tuple */[
                  param[/* v */1],
                  param[/* d */2]
                ];
        }
      } else {
        throw not_found;
      }
    }}

  function max_binding_opt(_param) {
    while(true) {
      var param = _param;
      if (param) {
        var r = param[/* r */3];
        if (r) {
          _param = r;
          continue ;
        } else {
          return /* tuple */[
                  param[/* v */1],
                  param[/* d */2]
                ];
        }
      } else {
        return ;
      }
    }}

  function remove_min_binding(param) {
    if (param) {
      var l = param[/* l */0];
      if (l) {
        return bal(remove_min_binding(l), param[/* v */1], param[/* d */2], param[/* r */3]);
      } else {
        return param[/* r */3];
      }
    } else {
      throw [
            invalid_argument,
            "Map.remove_min_elt"
          ];
    }
  }

  function merge(t1, t2) {
    if (t1) {
      if (t2) {
        var match = min_binding(t2);
        return bal(t1, match[0], match[1], remove_min_binding(t2));
      } else {
        return t1;
      }
    } else {
      return t2;
    }
  }

  function remove(x, m) {
    if (m) {
      var r = m[/* r */3];
      var d = m[/* d */2];
      var v = m[/* v */1];
      var l = m[/* l */0];
      var c = caml_string_compare(x, v);
      if (c === 0) {
        return merge(l, r);
      } else if (c < 0) {
        var ll = remove(x, l);
        if (l === ll) {
          return m;
        } else {
          return bal(ll, v, d, r);
        }
      } else {
        var rr = remove(x, r);
        if (r === rr) {
          return m;
        } else {
          return bal(l, v, d, rr);
        }
      }
    } else {
      return /* Empty */0;
    }
  }

  function update(x, f, m) {
    if (m) {
      var r = m[/* r */3];
      var d = m[/* d */2];
      var v = m[/* v */1];
      var l = m[/* l */0];
      var c = caml_string_compare(x, v);
      if (c === 0) {
        var match = _1(f, some(d));
        if (match !== undefined) {
          var data = valFromOption(match);
          if (d === data) {
            return m;
          } else {
            return /* Node */[
                    /* l */l,
                    /* v */x,
                    /* d */data,
                    /* r */r,
                    /* h */m[/* h */4]
                  ];
          }
        } else {
          return merge(l, r);
        }
      } else if (c < 0) {
        var ll = update(x, f, l);
        if (l === ll) {
          return m;
        } else {
          return bal(ll, v, d, r);
        }
      } else {
        var rr = update(x, f, r);
        if (r === rr) {
          return m;
        } else {
          return bal(l, v, d, rr);
        }
      }
    } else {
      var match$1 = _1(f, undefined);
      if (match$1 !== undefined) {
        return /* Node */[
                /* l : Empty */0,
                /* v */x,
                /* d */valFromOption(match$1),
                /* r : Empty */0,
                /* h */1
              ];
      } else {
        return /* Empty */0;
      }
    }
  }

  function iter$1(f, _param) {
    while(true) {
      var param = _param;
      if (param) {
        iter$1(f, param[/* l */0]);
        _2(f, param[/* v */1], param[/* d */2]);
        _param = param[/* r */3];
        continue ;
      } else {
        return /* () */0;
      }
    }}

  function map$2(f, param) {
    if (param) {
      var l$prime = map$2(f, param[/* l */0]);
      var d$prime = _1(f, param[/* d */2]);
      var r$prime = map$2(f, param[/* r */3]);
      return /* Node */[
              /* l */l$prime,
              /* v */param[/* v */1],
              /* d */d$prime,
              /* r */r$prime,
              /* h */param[/* h */4]
            ];
    } else {
      return /* Empty */0;
    }
  }

  function mapi(f, param) {
    if (param) {
      var v = param[/* v */1];
      var l$prime = mapi(f, param[/* l */0]);
      var d$prime = _2(f, v, param[/* d */2]);
      var r$prime = mapi(f, param[/* r */3]);
      return /* Node */[
              /* l */l$prime,
              /* v */v,
              /* d */d$prime,
              /* r */r$prime,
              /* h */param[/* h */4]
            ];
    } else {
      return /* Empty */0;
    }
  }

  function fold(f, _m, _accu) {
    while(true) {
      var accu = _accu;
      var m = _m;
      if (m) {
        _accu = _3(f, m[/* v */1], m[/* d */2], fold(f, m[/* l */0], accu));
        _m = m[/* r */3];
        continue ;
      } else {
        return accu;
      }
    }}

  function for_all(p, _param) {
    while(true) {
      var param = _param;
      if (param) {
        if (_2(p, param[/* v */1], param[/* d */2]) && for_all(p, param[/* l */0])) {
          _param = param[/* r */3];
          continue ;
        } else {
          return false;
        }
      } else {
        return true;
      }
    }}

  function exists(p, _param) {
    while(true) {
      var param = _param;
      if (param) {
        if (_2(p, param[/* v */1], param[/* d */2]) || exists(p, param[/* l */0])) {
          return true;
        } else {
          _param = param[/* r */3];
          continue ;
        }
      } else {
        return false;
      }
    }}

  function add_min_binding(k, x, param) {
    if (param) {
      return bal(add_min_binding(k, x, param[/* l */0]), param[/* v */1], param[/* d */2], param[/* r */3]);
    } else {
      return singleton(k, x);
    }
  }

  function add_max_binding(k, x, param) {
    if (param) {
      return bal(param[/* l */0], param[/* v */1], param[/* d */2], add_max_binding(k, x, param[/* r */3]));
    } else {
      return singleton(k, x);
    }
  }

  function join(l, v, d, r) {
    if (l) {
      if (r) {
        var rh = r[/* h */4];
        var lh = l[/* h */4];
        if (lh > (rh + 2 | 0)) {
          return bal(l[/* l */0], l[/* v */1], l[/* d */2], join(l[/* r */3], v, d, r));
        } else if (rh > (lh + 2 | 0)) {
          return bal(join(l, v, d, r[/* l */0]), r[/* v */1], r[/* d */2], r[/* r */3]);
        } else {
          return create$1(l, v, d, r);
        }
      } else {
        return add_max_binding(v, d, l);
      }
    } else {
      return add_min_binding(v, d, r);
    }
  }

  function concat$1(t1, t2) {
    if (t1) {
      if (t2) {
        var match = min_binding(t2);
        return join(t1, match[0], match[1], remove_min_binding(t2));
      } else {
        return t1;
      }
    } else {
      return t2;
    }
  }

  function concat_or_join(t1, v, d, t2) {
    if (d !== undefined) {
      return join(t1, v, valFromOption(d), t2);
    } else {
      return concat$1(t1, t2);
    }
  }

  function split(x, param) {
    if (param) {
      var r = param[/* r */3];
      var d = param[/* d */2];
      var v = param[/* v */1];
      var l = param[/* l */0];
      var c = caml_string_compare(x, v);
      if (c === 0) {
        return /* tuple */[
                l,
                some(d),
                r
              ];
      } else if (c < 0) {
        var match = split(x, l);
        return /* tuple */[
                match[0],
                match[1],
                join(match[2], v, d, r)
              ];
      } else {
        var match$1 = split(x, r);
        return /* tuple */[
                join(l, v, d, match$1[0]),
                match$1[1],
                match$1[2]
              ];
      }
    } else {
      return /* tuple */[
              /* Empty */0,
              undefined,
              /* Empty */0
            ];
    }
  }

  function merge$1(f, s1, s2) {
    if (s1) {
      var v1 = s1[/* v */1];
      if (s1[/* h */4] >= height(s2)) {
        var match = split(v1, s2);
        return concat_or_join(merge$1(f, s1[/* l */0], match[0]), v1, _3(f, v1, some(s1[/* d */2]), match[1]), merge$1(f, s1[/* r */3], match[2]));
      }
      
    } else if (!s2) {
      return /* Empty */0;
    }
    if (s2) {
      var v2 = s2[/* v */1];
      var match$1 = split(v2, s1);
      return concat_or_join(merge$1(f, match$1[0], s2[/* l */0]), v2, _3(f, v2, match$1[1], some(s2[/* d */2])), merge$1(f, match$1[2], s2[/* r */3]));
    } else {
      throw [
            assert_failure,
            /* tuple */[
              "map.ml",
              393,
              10
            ]
          ];
    }
  }

  function union(f, s1, s2) {
    if (s1) {
      if (s2) {
        var d2 = s2[/* d */2];
        var v2 = s2[/* v */1];
        var d1 = s1[/* d */2];
        var v1 = s1[/* v */1];
        if (s1[/* h */4] >= s2[/* h */4]) {
          var match = split(v1, s2);
          var d2$1 = match[1];
          var l = union(f, s1[/* l */0], match[0]);
          var r = union(f, s1[/* r */3], match[2]);
          if (d2$1 !== undefined) {
            return concat_or_join(l, v1, _3(f, v1, d1, valFromOption(d2$1)), r);
          } else {
            return join(l, v1, d1, r);
          }
        } else {
          var match$1 = split(v2, s1);
          var d1$1 = match$1[1];
          var l$1 = union(f, match$1[0], s2[/* l */0]);
          var r$1 = union(f, match$1[2], s2[/* r */3]);
          if (d1$1 !== undefined) {
            return concat_or_join(l$1, v2, _3(f, v2, valFromOption(d1$1), d2), r$1);
          } else {
            return join(l$1, v2, d2, r$1);
          }
        }
      } else {
        return s1;
      }
    } else {
      return s2;
    }
  }

  function filter$1(p, m) {
    if (m) {
      var r = m[/* r */3];
      var d = m[/* d */2];
      var v = m[/* v */1];
      var l = m[/* l */0];
      var l$prime = filter$1(p, l);
      var pvd = _2(p, v, d);
      var r$prime = filter$1(p, r);
      if (pvd) {
        if (l === l$prime && r === r$prime) {
          return m;
        } else {
          return join(l$prime, v, d, r$prime);
        }
      } else {
        return concat$1(l$prime, r$prime);
      }
    } else {
      return /* Empty */0;
    }
  }

  function partition(p, param) {
    if (param) {
      var d = param[/* d */2];
      var v = param[/* v */1];
      var match = partition(p, param[/* l */0]);
      var lf = match[1];
      var lt = match[0];
      var pvd = _2(p, v, d);
      var match$1 = partition(p, param[/* r */3]);
      var rf = match$1[1];
      var rt = match$1[0];
      if (pvd) {
        return /* tuple */[
                join(lt, v, d, rt),
                concat$1(lf, rf)
              ];
      } else {
        return /* tuple */[
                concat$1(lt, rt),
                join(lf, v, d, rf)
              ];
      }
    } else {
      return /* tuple */[
              /* Empty */0,
              /* Empty */0
            ];
    }
  }

  function cons_enum(_m, _e) {
    while(true) {
      var e = _e;
      var m = _m;
      if (m) {
        _e = /* More */[
          m[/* v */1],
          m[/* d */2],
          m[/* r */3],
          e
        ];
        _m = m[/* l */0];
        continue ;
      } else {
        return e;
      }
    }}

  function compare(cmp, m1, m2) {
    var _e1 = cons_enum(m1, /* End */0);
    var _e2 = cons_enum(m2, /* End */0);
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2) {
          var c = caml_string_compare(e1[0], e2[0]);
          if (c !== 0) {
            return c;
          } else {
            var c$1 = _2(cmp, e1[1], e2[1]);
            if (c$1 !== 0) {
              return c$1;
            } else {
              _e2 = cons_enum(e2[2], e2[3]);
              _e1 = cons_enum(e1[2], e1[3]);
              continue ;
            }
          }
        } else {
          return 1;
        }
      } else if (e2) {
        return -1;
      } else {
        return 0;
      }
    }}

  function equal$1(cmp, m1, m2) {
    var _e1 = cons_enum(m1, /* End */0);
    var _e2 = cons_enum(m2, /* End */0);
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2 && caml_string_compare(e1[0], e2[0]) === 0 && _2(cmp, e1[1], e2[1])) {
          _e2 = cons_enum(e2[2], e2[3]);
          _e1 = cons_enum(e1[2], e1[3]);
          continue ;
        } else {
          return false;
        }
      } else if (e2) {
        return false;
      } else {
        return true;
      }
    }}

  function cardinal(param) {
    if (param) {
      return (cardinal(param[/* l */0]) + 1 | 0) + cardinal(param[/* r */3]) | 0;
    } else {
      return 0;
    }
  }

  function bindings_aux(_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param) {
        _param = param[/* l */0];
        _accu = /* :: */[
          /* tuple */[
            param[/* v */1],
            param[/* d */2]
          ],
          bindings_aux(accu, param[/* r */3])
        ];
        continue ;
      } else {
        return accu;
      }
    }}

  function bindings(s) {
    return bindings_aux(/* [] */0, s);
  }

  var ObjectDict = {
    empty: /* Empty */0,
    is_empty: is_empty,
    mem: mem,
    add: add,
    update: update,
    singleton: singleton,
    remove: remove,
    merge: merge$1,
    union: union,
    compare: compare,
    equal: equal$1,
    iter: iter$1,
    fold: fold,
    for_all: for_all,
    exists: exists,
    filter: filter$1,
    partition: partition,
    cardinal: cardinal,
    bindings: bindings,
    min_binding: min_binding,
    min_binding_opt: min_binding_opt,
    max_binding: max_binding,
    max_binding_opt: max_binding_opt,
    choose: min_binding,
    choose_opt: min_binding_opt,
    split: split,
    find: find,
    find_opt: find_opt,
    find_first: find_first,
    find_first_opt: find_first_opt,
    find_last: find_last,
    find_last_opt: find_last_opt,
    map: map$2,
    mapi: mapi
  };

  var ParseFail = create("Tea_json.Decoder.ParseFail");

  var string = /* Decoder */[(function (value) {
        var match = classify$1(value);
        if (typeof match === "number" || match.tag) {
          return /* Error */__(1, ["Non-string value"]);
        } else {
          return /* Ok */__(0, [match[0]]);
        }
      })];

  var $$int = /* Decoder */[(function (value) {
        var match = classify$1(value);
        if (typeof match === "number" || match.tag !== /* JSONNumber */1) {
          return /* Error */__(1, ["Non-int value"]);
        } else {
          var n = match[0];
          if (n > min_int && n < max_int) {
            return /* Ok */__(0, [n | 0]);
          } else {
            return /* Error */__(1, ["number out of int range"]);
          }
        }
      })];

  var $$float = /* Decoder */[(function (value) {
        var match = classify$1(value);
        if (typeof match === "number" || match.tag !== /* JSONNumber */1) {
          return /* Error */__(1, ["Non-float-value"]);
        } else {
          return /* Ok */__(0, [match[0]]);
        }
      })];

  var bool = /* Decoder */[(function (value) {
        var match = classify$1(value);
        if (typeof match === "number") {
          switch (match) {
            case /* JSONFalse */0 :
                return /* Ok */__(0, [false]);
            case /* JSONTrue */1 :
                return /* Ok */__(0, [true]);
            case /* JSONNull */2 :
                return /* Error */__(1, ["Non-boolean value"]);
            
          }
        } else {
          return /* Error */__(1, ["Non-boolean value"]);
        }
      })];

  function $$null(v) {
    return /* Decoder */[(function (value) {
                var match = classify$1(value);
                if (typeof match === "number" && match >= 2) {
                  return /* Ok */__(0, [v]);
                } else {
                  return /* Error */__(1, ["Non-null value"]);
                }
              })];
  }

  function list(param) {
    var decoder = param[0];
    return /* Decoder */[(function (value) {
                var match = classify$1(value);
                if (typeof match === "number" || match.tag !== /* JSONArray */3) {
                  return /* Error */__(1, ["Non-list value"]);
                } else {
                  var parse = function (v) {
                    var match = _1(decoder, v);
                    if (match.tag) {
                      throw [
                            ParseFail,
                            match[0]
                          ];
                    } else {
                      return match[0];
                    }
                  };
                  try {
                    return /* Ok */__(0, [map(parse, to_list(match[0]))]);
                  }
                  catch (raw_exn){
                    var exn = internalToOCamlException(raw_exn);
                    if (exn[0] === ParseFail) {
                      return /* Error */__(1, ["list -> " + exn[1]]);
                    } else {
                      throw exn;
                    }
                  }
                }
              })];
  }

  function array(param) {
    var decoder = param[0];
    return /* Decoder */[(function (value) {
                var match = classify$1(value);
                if (typeof match === "number" || match.tag !== /* JSONArray */3) {
                  return /* Error */__(1, ["Non-array value"]);
                } else {
                  var parse = function (v) {
                    var match = _1(decoder, v);
                    if (match.tag) {
                      throw [
                            ParseFail,
                            match[0]
                          ];
                    } else {
                      return match[0];
                    }
                  };
                  try {
                    return /* Ok */__(0, [map$1(parse, match[0])]);
                  }
                  catch (raw_exn){
                    var exn = internalToOCamlException(raw_exn);
                    if (exn[0] === ParseFail) {
                      return /* Error */__(1, ["array -> " + exn[1]]);
                    } else {
                      throw exn;
                    }
                  }
                }
              })];
  }

  function keyValuePairs(param) {
    var decoder = param[0];
    return /* Decoder */[(function (value) {
                var match = classify$1(value);
                if (typeof match === "number" || match.tag !== /* JSONObject */2) {
                  return /* Error */__(1, ["Non-keyValuePair value"]);
                } else {
                  var o = match[0];
                  var keys = Object.keys(o);
                  var parse = function (k, l) {
                    var match = get(o, k);
                    if (match !== undefined) {
                      var match$1 = _1(decoder, valFromOption(match));
                      if (match$1.tag) {
                        throw [
                              ParseFail,
                              match$1[0]
                            ];
                      } else {
                        return /* :: */[
                                /* tuple */[
                                  k,
                                  match$1[0]
                                ],
                                l
                              ];
                      }
                    } else {
                      throw [
                            ParseFail,
                            "Key is undefined: " + k
                          ];
                    }
                  };
                  try {
                    return /* Ok */__(0, [fold_right$1(parse, keys, /* [] */0)]);
                  }
                  catch (raw_exn){
                    var exn = internalToOCamlException(raw_exn);
                    if (exn[0] === ParseFail) {
                      return /* Error */__(1, ["Invalid keyValuePair parsing: " + exn[1]]);
                    } else {
                      throw exn;
                    }
                  }
                }
              })];
  }

  function dict(param) {
    var decoder = param[0];
    return /* Decoder */[(function (value) {
                var match = classify$1(value);
                if (typeof match === "number" || match.tag !== /* JSONObject */2) {
                  return /* Error */__(1, ["Non-dict value"]);
                } else {
                  var o = match[0];
                  var keys = Object.keys(o);
                  var parse = function (k, d) {
                    var match = get(o, k);
                    if (match !== undefined) {
                      var match$1 = _1(decoder, valFromOption(match));
                      if (match$1.tag) {
                        throw [
                              ParseFail,
                              match$1[0]
                            ];
                      } else {
                        return add(k, match$1[0], d);
                      }
                    } else {
                      throw [
                            ParseFail,
                            "Key is undefined: " + k
                          ];
                    }
                  };
                  try {
                    return /* Ok */__(0, [fold_right$1(parse, keys, /* Empty */0)]);
                  }
                  catch (raw_exn){
                    var exn = internalToOCamlException(raw_exn);
                    if (exn[0] === ParseFail) {
                      return /* Error */__(1, ["Invalid dict parsing: " + exn[1]]);
                    } else {
                      throw exn;
                    }
                  }
                }
              })];
  }

  function field(key, param) {
    var decoder = param[0];
    return /* Decoder */[(function (value) {
                var match = classify$1(value);
                if (typeof match === "number" || match.tag !== /* JSONObject */2) {
                  return /* Error */__(1, ["Non-fieldable value"]);
                } else {
                  var match$1 = get(match[0], key);
                  if (match$1 !== undefined) {
                    var o = _1(decoder, valFromOption(match$1));
                    if (o.tag) {
                      return /* Error */__(1, ["field `" + (key + ("` -> " + o[0]))]);
                    } else {
                      return o;
                    }
                  } else {
                    return /* Error */__(1, ["Field Value is undefined: " + key]);
                  }
                }
              })];
  }

  function at(fields, dec) {
    return fold_right(field, fields, dec);
  }

  function index(idx, param) {
    var decoder = param[0];
    return /* Decoder */[(function (value) {
                var match = classify$1(value);
                if (typeof match === "number" || match.tag !== /* JSONArray */3) {
                  return /* Error */__(1, ["Non-array value"]);
                } else {
                  var a = match[0];
                  if (idx < 0 || idx > a.length) {
                    return /* Error */__(1, ["Array index out of range: " + String(idx)]);
                  } else {
                    return _1(decoder, caml_array_get(a, idx));
                  }
                }
              })];
  }

  function maybe(param) {
    var decoder = param[0];
    return /* Decoder */[(function (value) {
                var match = _1(decoder, value);
                if (match.tag) {
                  return /* Ok */__(0, [undefined]);
                } else {
                  return /* Ok */__(0, [some(match[0])]);
                }
              })];
  }

  function oneOf(decoders) {
    return /* Decoder */[(function (value) {
                var parse = function (v, _param) {
                  while(true) {
                    var param = _param;
                    if (param) {
                      var rest = param[1];
                      try {
                        var ok = _1(param[0][0], v);
                        if (ok.tag) {
                          return parse(v, rest);
                        } else {
                          return ok;
                        }
                      }
                      catch (exn){
                        _param = rest;
                        continue ;
                      }
                    } else {
                      return /* Error */__(1, ["No one-of's matched"]);
                    }
                  }              };
                return parse(value, decoders);
              })];
  }

  function map$1$1(mapper, param) {
    var decoder1 = param[0];
    return /* Decoder */[(function (value) {
                var match = _1(decoder1, value);
                if (match.tag) {
                  return /* Error */__(1, ["map " + match[0]]);
                } else {
                  return /* Ok */__(0, [_1(mapper, match[0])]);
                }
              })];
  }

  function map2(mapper, param, param$1) {
    var decoder2 = param$1[0];
    var decoder1 = param[0];
    return /* Decoder */[(function (value) {
                var match = _1(decoder1, value);
                var match$1 = _1(decoder2, value);
                if (!match.tag && !match$1.tag) {
                  return /* Ok */__(0, [_2(mapper, match[0], match$1[0])]);
                }
                var match$2 = error_of_first(match, match$1);
                if (match$2 !== undefined) {
                  return /* Error */__(1, ["map2 -> " + match$2]);
                } else {
                  throw [
                        failure,
                        "Impossible case"
                      ];
                }
              })];
  }

  function map3(mapper, param, param$1, param$2) {
    var decoder3 = param$2[0];
    var decoder2 = param$1[0];
    var decoder1 = param[0];
    return /* Decoder */[(function (value) {
                var match = _1(decoder1, value);
                var match$1 = _1(decoder2, value);
                var match$2 = _1(decoder3, value);
                if (!match.tag && !match$1.tag && !match$2.tag) {
                  return /* Ok */__(0, [_3(mapper, match[0], match$1[0], match$2[0])]);
                }
                var match$3 = first(match$2, first(match$1, match));
                if (match$3.tag) {
                  return /* Error */__(1, ["map3 -> " + match$3[0]]);
                } else {
                  throw [
                        failure,
                        "Impossible case"
                      ];
                }
              })];
  }

  function map4(mapper, param, param$1, param$2, param$3) {
    var decoder4 = param$3[0];
    var decoder3 = param$2[0];
    var decoder2 = param$1[0];
    var decoder1 = param[0];
    return /* Decoder */[(function (value) {
                var match = _1(decoder1, value);
                var match$1 = _1(decoder2, value);
                var match$2 = _1(decoder3, value);
                var match$3 = _1(decoder4, value);
                if (!match.tag && !match$1.tag && !match$2.tag && !match$3.tag) {
                  return /* Ok */__(0, [_4(mapper, match[0], match$1[0], match$2[0], match$3[0])]);
                }
                var match$4 = first(match$3, first(match$2, first(match$1, match)));
                if (match$4.tag) {
                  return /* Error */__(1, ["map4 -> " + match$4[0]]);
                } else {
                  throw [
                        failure,
                        "Impossible case"
                      ];
                }
              })];
  }

  function map5(mapper, param, param$1, param$2, param$3, param$4) {
    var decoder5 = param$4[0];
    var decoder4 = param$3[0];
    var decoder3 = param$2[0];
    var decoder2 = param$1[0];
    var decoder1 = param[0];
    return /* Decoder */[(function (value) {
                var match = _1(decoder1, value);
                var match$1 = _1(decoder2, value);
                var match$2 = _1(decoder3, value);
                var match$3 = _1(decoder4, value);
                var match$4 = _1(decoder5, value);
                if (!match.tag && !match$1.tag && !match$2.tag && !match$3.tag && !match$4.tag) {
                  return /* Ok */__(0, [_5(mapper, match[0], match$1[0], match$2[0], match$3[0], match$4[0])]);
                }
                var match$5 = first(match$4, first(match$3, first(match$2, first(match$1, match))));
                if (match$5.tag) {
                  return /* Error */__(1, ["map5 -> " + match$5[0]]);
                } else {
                  throw [
                        failure,
                        "Impossible case"
                      ];
                }
              })];
  }

  function map6(mapper, param, param$1, param$2, param$3, param$4, param$5) {
    var decoder6 = param$5[0];
    var decoder5 = param$4[0];
    var decoder4 = param$3[0];
    var decoder3 = param$2[0];
    var decoder2 = param$1[0];
    var decoder1 = param[0];
    return /* Decoder */[(function (value) {
                var match = _1(decoder1, value);
                var match$1 = _1(decoder2, value);
                var match$2 = _1(decoder3, value);
                var match$3 = _1(decoder4, value);
                var match$4 = _1(decoder5, value);
                var match$5 = _1(decoder6, value);
                if (!match.tag && !match$1.tag && !match$2.tag && !match$3.tag && !match$4.tag && !match$5.tag) {
                  return /* Ok */__(0, [_6(mapper, match[0], match$1[0], match$2[0], match$3[0], match$4[0], match$5[0])]);
                }
                var match$6 = first(match$5, first(match$4, first(match$3, first(match$2, first(match$1, match)))));
                if (match$6.tag) {
                  return /* Error */__(1, ["map6 -> " + match$6[0]]);
                } else {
                  throw [
                        failure,
                        "Impossible case"
                      ];
                }
              })];
  }

  function map7(mapper, param, param$1, param$2, param$3, param$4, param$5, param$6) {
    var decoder7 = param$6[0];
    var decoder6 = param$5[0];
    var decoder5 = param$4[0];
    var decoder4 = param$3[0];
    var decoder3 = param$2[0];
    var decoder2 = param$1[0];
    var decoder1 = param[0];
    return /* Decoder */[(function (value) {
                var match = _1(decoder1, value);
                var match$1 = _1(decoder2, value);
                var match$2 = _1(decoder3, value);
                var match$3 = _1(decoder4, value);
                var match$4 = _1(decoder5, value);
                var match$5 = _1(decoder6, value);
                var match$6 = _1(decoder7, value);
                if (!match.tag && !match$1.tag && !match$2.tag && !match$3.tag && !match$4.tag && !match$5.tag && !match$6.tag) {
                  return /* Ok */__(0, [_7(mapper, match[0], match$1[0], match$2[0], match$3[0], match$4[0], match$5[0], match$6[0])]);
                }
                var match$7 = first(match$6, first(match$5, first(match$4, first(match$3, first(match$2, first(match$1, match))))));
                if (match$7.tag) {
                  return /* Error */__(1, ["map7 -> " + match$7[0]]);
                } else {
                  throw [
                        failure,
                        "Impossible case"
                      ];
                }
              })];
  }

  function map8(mapper, param, param$1, param$2, param$3, param$4, param$5, param$6, param$7) {
    var decoder8 = param$7[0];
    var decoder7 = param$6[0];
    var decoder6 = param$5[0];
    var decoder5 = param$4[0];
    var decoder4 = param$3[0];
    var decoder3 = param$2[0];
    var decoder2 = param$1[0];
    var decoder1 = param[0];
    return /* Decoder */[(function (value) {
                var match = _1(decoder1, value);
                var match$1 = _1(decoder2, value);
                var match$2 = _1(decoder3, value);
                var match$3 = _1(decoder4, value);
                var match$4 = _1(decoder5, value);
                var match$5 = _1(decoder6, value);
                var match$6 = _1(decoder7, value);
                var match$7 = _1(decoder8, value);
                if (!match.tag && !match$1.tag && !match$2.tag && !match$3.tag && !match$4.tag && !match$5.tag && !match$6.tag && !match$7.tag) {
                  return /* Ok */__(0, [_8(mapper, match[0], match$1[0], match$2[0], match$3[0], match$4[0], match$5[0], match$6[0], match$7[0])]);
                }
                var match$8 = first(match$7, first(match$6, first(match$5, first(match$4, first(match$3, first(match$2, first(match$1, match)))))));
                if (match$8.tag) {
                  return /* Error */__(1, ["map8 -> " + match$8[0]]);
                } else {
                  throw [
                        failure,
                        "Impossible case"
                      ];
                }
              })];
  }

  function succeed(v) {
    return /* Decoder */[(function (_value) {
                return /* Ok */__(0, [v]);
              })];
  }

  function fail(e) {
    return /* Decoder */[(function (_value) {
                return /* Error */__(1, [e]);
              })];
  }

  var value = /* Decoder */[(function (value) {
        return /* Ok */__(0, [value]);
      })];

  function andThen(func, param) {
    var decoder = param[0];
    return /* Decoder */[(function (value) {
                var err = _1(decoder, value);
                if (err.tag) {
                  return err;
                } else {
                  var match = _1(func, err[0]);
                  return _1(match[0], value);
                }
              })];
  }

  function lazy_(func) {
    return andThen(func, /* Decoder */[(function (_value) {
                    return /* Ok */__(0, [/* () */0]);
                  })]);
  }

  function nullable(decoder) {
    return oneOf(/* :: */[
                $$null(undefined),
                /* :: */[
                  map$1$1((function (v) {
                          return some(v);
                        }), decoder),
                  /* [] */0
                ]
              ]);
  }

  function decodeValue(param, value) {
    try {
      return _1(param[0], value);
    }
    catch (raw_exn){
      var exn = internalToOCamlException(raw_exn);
      if (exn[0] === ParseFail) {
        return /* Error */__(1, [exn[1]]);
      } else {
        return /* Error */__(1, ["Unknown JSON parsing error"]);
      }
    }
  }

  function decodeEvent(param, value) {
    try {
      return _1(param[0], value);
    }
    catch (raw_exn){
      var exn = internalToOCamlException(raw_exn);
      if (exn[0] === ParseFail) {
        return /* Error */__(1, [exn[1]]);
      } else {
        return /* Error */__(1, ["Unknown JSON parsing error"]);
      }
    }
  }

  function decodeString(decoder, string) {
    try {
      var value = JSON.parse(string);
      return decodeValue(decoder, value);
    }
    catch (exn){
      return /* Error */__(1, ["Invalid JSON string"]);
    }
  }

  var Decoder = {
    ObjectDict: ObjectDict,
    ParseFail: ParseFail,
    string: string,
    $$int: $$int,
    $$float: $$float,
    bool: bool,
    $$null: $$null,
    list: list,
    array: array,
    keyValuePairs: keyValuePairs,
    dict: dict,
    field: field,
    at: at,
    index: index,
    maybe: maybe,
    oneOf: oneOf,
    map: map$1$1,
    map2: map2,
    map3: map3,
    map4: map4,
    map5: map5,
    map6: map6,
    map7: map7,
    map8: map8,
    succeed: succeed,
    fail: fail,
    value: value,
    andThen: andThen,
    lazy_: lazy_,
    nullable: nullable,
    decodeValue: decodeValue,
    decodeEvent: decodeEvent,
    decodeString: decodeString
  };
  /* No side effect */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function text(str) {
    return /* Text */__(1, [str]);
  }

  function div(keyOpt, uniqueOpt, props, nodes) {
    var key = keyOpt !== undefined ? keyOpt : "";
    var unique = uniqueOpt !== undefined ? uniqueOpt : "";
    return fullnode("", "div", key, unique, props, nodes);
  }

  function button(keyOpt, uniqueOpt, props, nodes) {
    var key = keyOpt !== undefined ? keyOpt : "";
    var unique = uniqueOpt !== undefined ? uniqueOpt : "";
    return fullnode("", "button", key, unique, props, nodes);
  }

  function input$prime(keyOpt, uniqueOpt, props, nodes) {
    var key = keyOpt !== undefined ? keyOpt : "";
    var unique = uniqueOpt !== undefined ? uniqueOpt : "";
    return fullnode("", "input", key, unique, props, nodes);
  }

  function label(keyOpt, uniqueOpt, props, nodes) {
    var key = keyOpt !== undefined ? keyOpt : "";
    var unique = uniqueOpt !== undefined ? uniqueOpt : "";
    return fullnode("", "label", key, unique, props, nodes);
  }

  function select(keyOpt, uniqueOpt, props, nodes) {
    var key = keyOpt !== undefined ? keyOpt : "";
    var unique = uniqueOpt !== undefined ? uniqueOpt : "";
    return fullnode("", "select", key, unique, props, nodes);
  }

  function option$prime(keyOpt, uniqueOpt, props, nodes) {
    var key = keyOpt !== undefined ? keyOpt : "";
    var unique = uniqueOpt !== undefined ? uniqueOpt : "";
    return fullnode("", "option", key, unique, props, nodes);
  }

  function form(keyOpt, uniqueOpt, props, nodes) {
    var key = keyOpt !== undefined ? keyOpt : "";
    var unique = uniqueOpt !== undefined ? uniqueOpt : "";
    return fullnode("", "form", key, unique, props, nodes);
  }

  function id$1(str) {
    return /* RawProp */__(0, [
              "id",
              str
            ]);
  }

  function class$prime(name) {
    return /* RawProp */__(0, [
              "className",
              name
            ]);
  }

  function type$prime(typ) {
    return /* RawProp */__(0, [
              "type",
              typ
            ]);
  }

  function value$1(str) {
    return /* RawProp */__(0, [
              "value",
              str
            ]);
  }

  function name(str) {
    return /* RawProp */__(0, [
              "name",
              str
            ]);
  }

  function for$prime(str) {
    return /* RawProp */__(0, [
              "htmlFor",
              str
            ]);
  }

  function action(a) {
    return /* RawProp */__(0, [
              "action",
              a
            ]);
  }

  function method$prime(m) {
    return /* RawProp */__(0, [
              "method",
              m
            ]);
  }

  var targetValue = Decoder.at(/* :: */[
        "target",
        /* :: */[
          "value",
          /* [] */0
        ]
      ], Decoder.string);

  var targetChecked = Decoder.at(/* :: */[
        "target",
        /* :: */[
          "checked",
          /* [] */0
        ]
      ], Decoder.bool);

  var keyCode = Decoder.field("keyCode", Decoder.$$int);

  function max(value) {
    return /* Attribute */__(1, [
              "",
              "max",
              value
            ]);
  }

  function min(value) {
    return /* Attribute */__(1, [
              "",
              "min",
              value
            ]);
  }

  function step(value) {
    return /* Attribute */__(1, [
              "",
              "step",
              value
            ]);
  }

  function disabled(b) {
    if (b) {
      return /* Attribute */__(1, [
                "",
                "disabled",
                "true"
              ]);
    } else {
      return /* NoProp */0;
    }
  }

  function selected(b) {
    if (b) {
      return /* Attribute */__(1, [
                "",
                "selected",
                "true"
              ]);
    } else {
      return /* NoProp */0;
    }
  }

  function acceptCharset(c) {
    return /* Attribute */__(1, [
              "",
              "accept-charset",
              c
            ]);
  }

  function rel(value) {
    return /* Attribute */__(1, [
              "",
              "rel",
              value
            ]);
  }

  var Attributes = {
    max: max,
    min: min,
    step: step,
    disabled: disabled,
    selected: selected,
    acceptCharset: acceptCharset,
    rel: rel
  };
  /* targetValue Not a pure module */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function labeled_input(label_name, attrs) {
    return div(undefined, undefined, /* :: */[
                class$prime("labeledInput"),
                /* [] */0
              ], /* :: */[
                label(undefined, undefined, /* :: */[
                      for$prime(label_name),
                      /* [] */0
                    ], /* :: */[
                      text(label_name),
                      /* [] */0
                    ]),
                /* :: */[
                  input$prime(undefined, undefined, /* :: */[
                        name(label_name),
                        attrs
                      ], /* [] */0),
                  /* [] */0
                ]
              ]);
  }

  function labeled_select(select_name, options) {
    return div(undefined, undefined, /* :: */[
                class$prime("labeledInput"),
                /* [] */0
              ], /* :: */[
                label(undefined, undefined, /* :: */[
                      for$prime(select_name),
                      /* [] */0
                    ], /* :: */[
                      text(select_name),
                      /* [] */0
                    ]),
                /* :: */[
                  select(undefined, undefined, /* :: */[
                        id$1(select_name),
                        /* :: */[
                          name(select_name),
                          /* [] */0
                        ]
                      ], map((function (param) {
                              var name = param[0];
                              return option$prime(undefined, undefined, /* :: */[
                                          value$1(name),
                                          /* :: */[
                                            Attributes.selected(param[1]),
                                            /* [] */0
                                          ]
                                        ], /* :: */[
                                          text(name),
                                          /* [] */0
                                        ]);
                            }), options)),
                  /* [] */0
                ]
              ]);
  }

  function labeled_tz_select(select_name, user_tz) {
    return labeled_select(select_name, map((function (tzname) {
                      return /* tuple */[
                              tzname,
                              user_tz !== undefined ? equal(user_tz, tzname) : false
                            ];
                    }), tz_labels));
  }

  function form_block(body) {
    return div(undefined, undefined, /* :: */[
                class$prime("formBlock"),
                /* [] */0
              ], body);
  }
  /* Tea_html Not a pure module */

  // Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

  function init(tz) {
    if (tz === "") {
      return {
              user_tz: undefined
            };
    } else {
      return {
              user_tz: tz
            };
    }
  }

  function update$1(model, param) {
    return model;
  }

  function view(model) {
    return form(undefined, undefined, /* :: */[
                method$prime("post"),
                /* :: */[
                  action("/event/new"),
                  /* [] */0
                ]
              ], /* :: */[
                form_block(/* :: */[
                      labeled_input("Summary", /* [] */0),
                      /* :: */[
                        labeled_input("Date", /* :: */[
                              type$prime("date"),
                              /* [] */0
                            ]),
                        /* :: */[
                          labeled_input("Start Time", /* :: */[
                                type$prime("time"),
                                /* [] */0
                              ]),
                          /* :: */[
                            labeled_input("End Time", /* :: */[
                                  type$prime("time"),
                                  /* [] */0
                                ]),
                            /* :: */[
                              labeled_tz_select("Time Zone", model.user_tz),
                              /* :: */[
                                labeled_select("Repeats", /* :: */[
                                      /* tuple */[
                                        "Never",
                                        true
                                      ],
                                      map((function (name) {
                                              return /* tuple */[
                                                      name,
                                                      false
                                                    ];
                                            }), /* :: */[
                                            "Daily",
                                            /* :: */[
                                              "Weekly",
                                              /* :: */[
                                                "Monthly",
                                                /* :: */[
                                                  "Yearly",
                                                  /* [] */0
                                                ]
                                              ]
                                            ]
                                          ])
                                    ]),
                                /* [] */0
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]),
                /* :: */[
                  button(undefined, undefined, /* :: */[
                        type$prime("submit"),
                        /* [] */0
                      ], /* :: */[
                        text("Create"),
                        /* [] */0
                      ]),
                  /* [] */0
                ]
              ]);
  }

  function main(user_tz) {
    var partial_arg_model = init(user_tz);
    var partial_arg = {
      model: partial_arg_model,
      update: update$1,
      view: view
    };
    return (function (param, param$1) {
        return beginnerProgram(partial_arg, param);
      });
  }
  /* Common Not a pure module */

  document.addEventListener('DOMContentLoaded', function() {
    var elem = document.getElementById('bs-form');
    //var formId = elem.attributes['data-sandcal-form-id'].nodeValue;
    var userTz = elem.attributes['data-sandcal-user-tz'].nodeValue;
    main(userTz)(elem);
  });

}());
