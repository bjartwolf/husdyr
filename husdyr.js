#!/usr/bin/env node
function e(a) {
  throw a;
}
var g = void 0, k = !0, l = null, p = !1;
function aa() {
  return function(a) {
    return a
  }
}
function q(a) {
  return function() {
    return this[a]
  }
}
function t(a) {
  return function() {
    return a
  }
}
var w;
function x(a) {
  var b = typeof a;
  if("object" == b) {
    if(a) {
      if(a instanceof Array) {
        return"array"
      }
      if(a instanceof Object) {
        return b
      }
      var c = Object.prototype.toString.call(a);
      if("[object Window]" == c) {
        return"object"
      }
      if("[object Array]" == c || "number" == typeof a.length && "undefined" != typeof a.splice && "undefined" != typeof a.propertyIsEnumerable && !a.propertyIsEnumerable("splice")) {
        return"array"
      }
      if("[object Function]" == c || "undefined" != typeof a.call && "undefined" != typeof a.propertyIsEnumerable && !a.propertyIsEnumerable("call")) {
        return"function"
      }
    }else {
      return"null"
    }
  }else {
    if("function" == b && "undefined" == typeof a.call) {
      return"object"
    }
  }
  return b
}
function ba(a) {
  return"string" == typeof a
}
var ca = "closure_uid_" + Math.floor(2147483648 * Math.random()).toString(36), da = 0;
function ea(a) {
  for(var b = 0, c = 0;c < a.length;++c) {
    b = 31 * b + a.charCodeAt(c), b %= 4294967296
  }
  return b
}
;var fa = Array.prototype;
function ga(a, b) {
  return a > b ? 1 : a < b ? -1 : 0
}
;function ja(a, b) {
  var c = Array.prototype.slice.call(arguments), d = c.shift();
  "undefined" == typeof d && e(Error("[goog.string.format] Template required"));
  return d.replace(/%([0\-\ \+]*)(\d+)?(\.(\d+))?([%sfdiu])/g, function(a, b, d, j, m, n, s, r) {
    if("%" == n) {
      return"%"
    }
    var v = c.shift();
    "undefined" == typeof v && e(Error("[goog.string.format] Not enough arguments"));
    arguments[0] = v;
    return ja.ba[n].apply(l, arguments)
  })
}
ja.ba = {};
ja.ba.s = function(a, b, c) {
  return isNaN(c) || "" == c || a.length >= c ? a : a = -1 < b.indexOf("-", 0) ? a + Array(c - a.length + 1).join(" ") : Array(c - a.length + 1).join(" ") + a
};
ja.ba.f = function(a, b, c, d, f) {
  d = a.toString();
  isNaN(f) || "" == f || (d = a.toFixed(f));
  var h;
  h = 0 > a ? "-" : 0 <= b.indexOf("+") ? "+" : 0 <= b.indexOf(" ") ? " " : "";
  0 <= a && (d = h + d);
  if(isNaN(c) || d.length >= c) {
    return d
  }
  d = isNaN(f) ? Math.abs(a).toString() : Math.abs(a).toFixed(f);
  a = c - d.length - h.length;
  return d = 0 <= b.indexOf("-", 0) ? h + d + Array(a + 1).join(" ") : h + Array(a + 1).join(0 <= b.indexOf("0", 0) ? "0" : " ") + d
};
ja.ba.d = function(a, b, c, d, f, h, i, j) {
  return ja.ba.f(parseInt(a, 10), b, c, d, 0, h, i, j)
};
ja.ba.i = ja.ba.d;
ja.ba.u = ja.ba.d;
function ka(a, b) {
  a != l && this.append.apply(this, arguments)
}
ka.prototype.ua = "";
ka.prototype.append = function(a, b, c) {
  this.ua += a;
  if(b != l) {
    for(var d = 1;d < arguments.length;d++) {
      this.ua += arguments[d]
    }
  }
  return this
};
ka.prototype.toString = q("ua");
var la;
function ma() {
  e(Error("No *print-fn* fn set for evaluation environment"))
}
function oa(a) {
  return ma = a
}
var pa = ["cljs", "core", "set_print_fn_BANG_"], qa = this;
!(pa[0] in qa) && qa.execScript && qa.execScript("var " + pa[0]);
for(var ra;pa.length && (ra = pa.shift());) {
  !pa.length && oa !== g ? qa[ra] = oa : qa = qa[ra] ? qa[ra] : qa[ra] = {}
}
function sa() {
  return ta(["\ufdd0:flush-on-newline", k, "\ufdd0:readably", k, "\ufdd0:meta", p, "\ufdd0:dup", p], k)
}
function y(a) {
  return a != l && a !== p
}
function va(a) {
  return"number" === typeof a
}
function wa(a) {
  var b = ba(a);
  return b ? "\ufdd0" !== a.charAt(0) : b
}
function z(a, b) {
  return a[x(b == l ? l : b)] ? k : a._ ? k : p
}
var xa = l;
function A(a, b) {
  var c = b == l ? l : b.constructor, c = y(y(c) ? c.$a : c) ? c.lb : x(b);
  return Error(["No protocol method ", a, " defined for type ", c, ": ", b].join(""))
}
function ya(a) {
  return Array.prototype.slice.call(arguments)
}
var Aa = {}, Ba = {};
function Ca(a) {
  if(a ? a.B : a) {
    return a.B(a)
  }
  var b;
  var c = Ca[x(a == l ? l : a)];
  c ? b = c : (c = Ca._) ? b = c : e(A("ICounted.-count", a));
  return b.call(l, a)
}
function Da(a, b) {
  if(a ? a.A : a) {
    return a.A(a, b)
  }
  var c;
  var d = Da[x(a == l ? l : a)];
  d ? c = d : (d = Da._) ? c = d : e(A("ICollection.-conj", a));
  return c.call(l, a, b)
}
var Ea = {}, B, Fa = l;
function Ga(a, b) {
  if(a ? a.M : a) {
    return a.M(a, b)
  }
  var c;
  var d = B[x(a == l ? l : a)];
  d ? c = d : (d = B._) ? c = d : e(A("IIndexed.-nth", a));
  return c.call(l, a, b)
}
function Ha(a, b, c) {
  if(a ? a.V : a) {
    return a.V(a, b, c)
  }
  var d;
  var f = B[x(a == l ? l : a)];
  f ? d = f : (f = B._) ? d = f : e(A("IIndexed.-nth", a));
  return d.call(l, a, b, c)
}
Fa = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return Ga.call(this, a, b);
    case 3:
      return Ha.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Fa.a = Ga;
Fa.c = Ha;
B = Fa;
function C(a) {
  if(a ? a.O : a) {
    return a.O(a)
  }
  var b;
  var c = C[x(a == l ? l : a)];
  c ? b = c : (c = C._) ? b = c : e(A("ISeq.-first", a));
  return b.call(l, a)
}
function D(a) {
  if(a ? a.P : a) {
    return a.P(a)
  }
  var b;
  var c = D[x(a == l ? l : a)];
  c ? b = c : (c = D._) ? b = c : e(A("ISeq.-rest", a));
  return b.call(l, a)
}
var Ia = {}, Ja, Ka = l;
function La(a, b) {
  if(a ? a.F : a) {
    return a.F(a, b)
  }
  var c;
  var d = Ja[x(a == l ? l : a)];
  d ? c = d : (d = Ja._) ? c = d : e(A("ILookup.-lookup", a));
  return c.call(l, a, b)
}
function Ma(a, b, c) {
  if(a ? a.t : a) {
    return a.t(a, b, c)
  }
  var d;
  var f = Ja[x(a == l ? l : a)];
  f ? d = f : (f = Ja._) ? d = f : e(A("ILookup.-lookup", a));
  return d.call(l, a, b, c)
}
Ka = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return La.call(this, a, b);
    case 3:
      return Ma.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Ka.a = La;
Ka.c = Ma;
Ja = Ka;
function Na(a, b) {
  if(a ? a.ya : a) {
    return a.ya(a, b)
  }
  var c;
  var d = Na[x(a == l ? l : a)];
  d ? c = d : (d = Na._) ? c = d : e(A("IAssociative.-contains-key?", a));
  return c.call(l, a, b)
}
function Oa(a, b, c) {
  if(a ? a.da : a) {
    return a.da(a, b, c)
  }
  var d;
  var f = Oa[x(a == l ? l : a)];
  f ? d = f : (f = Oa._) ? d = f : e(A("IAssociative.-assoc", a));
  return d.call(l, a, b, c)
}
var Pa = {}, Qa = {};
function Ra(a) {
  if(a ? a.Wa : a) {
    return a.Wa(a)
  }
  var b;
  var c = Ra[x(a == l ? l : a)];
  c ? b = c : (c = Ra._) ? b = c : e(A("IMapEntry.-key", a));
  return b.call(l, a)
}
function Sa(a) {
  if(a ? a.Xa : a) {
    return a.Xa(a)
  }
  var b;
  var c = Sa[x(a == l ? l : a)];
  c ? b = c : (c = Sa._) ? b = c : e(A("IMapEntry.-val", a));
  return b.call(l, a)
}
var Ua = {}, Va = {}, Wa = {};
function Xa(a) {
  if(a ? a.K : a) {
    return a.K(a)
  }
  var b;
  var c = Xa[x(a == l ? l : a)];
  c ? b = c : (c = Xa._) ? b = c : e(A("IMeta.-meta", a));
  return b.call(l, a)
}
var Ya = {};
function Za(a, b) {
  if(a ? a.I : a) {
    return a.I(a, b)
  }
  var c;
  var d = Za[x(a == l ? l : a)];
  d ? c = d : (d = Za._) ? c = d : e(A("IWithMeta.-with-meta", a));
  return c.call(l, a, b)
}
var $a = {}, ab, bb = l;
function cb(a, b) {
  if(a ? a.na : a) {
    return a.na(a, b)
  }
  var c;
  var d = ab[x(a == l ? l : a)];
  d ? c = d : (d = ab._) ? c = d : e(A("IReduce.-reduce", a));
  return c.call(l, a, b)
}
function db(a, b, c) {
  if(a ? a.oa : a) {
    return a.oa(a, b, c)
  }
  var d;
  var f = ab[x(a == l ? l : a)];
  f ? d = f : (f = ab._) ? d = f : e(A("IReduce.-reduce", a));
  return d.call(l, a, b, c)
}
bb = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return cb.call(this, a, b);
    case 3:
      return db.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
bb.a = cb;
bb.c = db;
ab = bb;
function eb(a, b) {
  if(a ? a.C : a) {
    return a.C(a, b)
  }
  var c;
  var d = eb[x(a == l ? l : a)];
  d ? c = d : (d = eb._) ? c = d : e(A("IEquiv.-equiv", a));
  return c.call(l, a, b)
}
function fb(a) {
  if(a ? a.G : a) {
    return a.G(a)
  }
  var b;
  var c = fb[x(a == l ? l : a)];
  c ? b = c : (c = fb._) ? b = c : e(A("IHash.-hash", a));
  return b.call(l, a)
}
function gb(a) {
  if(a ? a.z : a) {
    return a.z(a)
  }
  var b;
  var c = gb[x(a == l ? l : a)];
  c ? b = c : (c = gb._) ? b = c : e(A("ISeqable.-seq", a));
  return b.call(l, a)
}
var kb = {};
function F(a, b) {
  if(a ? a.Za : a) {
    return a.Za(0, b)
  }
  var c;
  var d = F[x(a == l ? l : a)];
  d ? c = d : (d = F._) ? c = d : e(A("IWriter.-write", a));
  return c.call(l, a, b)
}
function lb(a) {
  if(a ? a.jb : a) {
    return l
  }
  var b;
  var c = lb[x(a == l ? l : a)];
  c ? b = c : (c = lb._) ? b = c : e(A("IWriter.-flush", a));
  return b.call(l, a)
}
function mb(a) {
  if(a ? a.la : a) {
    return a.la(a)
  }
  var b;
  var c = mb[x(a == l ? l : a)];
  c ? b = c : (c = mb._) ? b = c : e(A("IEditableCollection.-as-transient", a));
  return b.call(l, a)
}
function nb(a, b) {
  if(a ? a.qa : a) {
    return a.qa(a, b)
  }
  var c;
  var d = nb[x(a == l ? l : a)];
  d ? c = d : (d = nb._) ? c = d : e(A("ITransientCollection.-conj!", a));
  return c.call(l, a, b)
}
function ob(a) {
  if(a ? a.va : a) {
    return a.va(a)
  }
  var b;
  var c = ob[x(a == l ? l : a)];
  c ? b = c : (c = ob._) ? b = c : e(A("ITransientCollection.-persistent!", a));
  return b.call(l, a)
}
function pb(a, b, c) {
  if(a ? a.pa : a) {
    return a.pa(a, b, c)
  }
  var d;
  var f = pb[x(a == l ? l : a)];
  f ? d = f : (f = pb._) ? d = f : e(A("ITransientAssociative.-assoc!", a));
  return d.call(l, a, b, c)
}
function qb(a) {
  if(a ? a.Ta : a) {
    return a.Ta()
  }
  var b;
  var c = qb[x(a == l ? l : a)];
  c ? b = c : (c = qb._) ? b = c : e(A("IChunk.-drop-first", a));
  return b.call(l, a)
}
function rb(a) {
  if(a ? a.Ca : a) {
    return a.Ca(a)
  }
  var b;
  var c = rb[x(a == l ? l : a)];
  c ? b = c : (c = rb._) ? b = c : e(A("IChunkedSeq.-chunked-first", a));
  return b.call(l, a)
}
function sb(a) {
  if(a ? a.za : a) {
    return a.za(a)
  }
  var b;
  var c = sb[x(a == l ? l : a)];
  c ? b = c : (c = sb._) ? b = c : e(A("IChunkedSeq.-chunked-rest", a));
  return b.call(l, a)
}
function tb(a) {
  this.nb = a;
  this.r = 0;
  this.j = 1073741824
}
tb.prototype.Za = function(a, b) {
  return this.nb.append(b)
};
tb.prototype.jb = t(l);
function ub(a) {
  var b = new ka, c = new tb(b);
  a.H(a, c, sa());
  lb(c);
  return"" + G(b)
}
function H(a) {
  if(a == l) {
    return l
  }
  var b;
  if(b = a) {
    b = (b = a.j & 8388608) ? b : a.vb
  }
  if(b) {
    return a.z(a)
  }
  if(a instanceof Array || wa(a)) {
    return 0 === a.length ? l : new vb(a, 0)
  }
  if(z(Ia, a)) {
    return gb(a)
  }
  e(Error([G(a), G("is not ISeqable")].join("")))
}
function J(a) {
  if(a == l) {
    return l
  }
  var b;
  if(b = a) {
    b = (b = a.j & 64) ? b : a.Ya
  }
  if(b) {
    return a.O(a)
  }
  a = H(a);
  return a == l ? l : C(a)
}
function K(a) {
  if(a != l) {
    var b;
    if(b = a) {
      b = (b = a.j & 64) ? b : a.Ya
    }
    if(b) {
      return a.P(a)
    }
    a = H(a);
    return a != l ? D(a) : wb
  }
  return wb
}
function L(a) {
  if(a == l) {
    a = l
  }else {
    var b;
    if(b = a) {
      b = (b = a.j & 128) ? b : a.ub
    }
    a = b ? a.ea(a) : H(K(a))
  }
  return a
}
var O, xb = l;
function yb(a, b) {
  var c = a === b;
  return c ? c : eb(a, b)
}
function zb(a, b, c) {
  for(;;) {
    if(y(xb.a(a, b))) {
      if(L(c)) {
        a = b, b = J(c), c = L(c)
      }else {
        return xb.a(b, J(c))
      }
    }else {
      return p
    }
  }
}
function Ab(a, b, c) {
  var d = l;
  2 < arguments.length && (d = R(Array.prototype.slice.call(arguments, 2), 0));
  return zb.call(this, a, b, d)
}
Ab.n = 2;
Ab.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = K(a);
  return zb(b, c, a)
};
Ab.g = zb;
xb = function(a, b, c) {
  switch(arguments.length) {
    case 1:
      return k;
    case 2:
      return yb.call(this, a, b);
    default:
      return Ab.g(a, b, R(arguments, 2))
  }
  e(Error("Invalid arity: " + arguments.length))
};
xb.n = 2;
xb.k = Ab.k;
xb.e = t(k);
xb.a = yb;
xb.g = Ab.g;
O = xb;
fb["null"] = t(0);
Ua["null"] = k;
Ba["null"] = k;
Ca["null"] = t(0);
eb["null"] = function(a, b) {
  return b == l
};
Ya["null"] = k;
Za["null"] = t(l);
Wa["null"] = k;
Xa["null"] = t(l);
Pa["null"] = k;
Date.prototype.C = function(a, b) {
  var c = b instanceof Date;
  return c ? a.toString() === b.toString() : c
};
fb.number = function(a) {
  return Math.floor(a) % 2147483647
};
eb.number = function(a, b) {
  return a === b
};
fb["boolean"] = function(a) {
  return a === k ? 1 : 0
};
Wa["function"] = k;
Xa["function"] = t(l);
Aa["function"] = k;
fb._ = function(a) {
  return a[ca] || (a[ca] = ++da)
};
var Eb, Fb = l;
function Gb(a, b) {
  var c = Ca(a);
  if(0 === c) {
    return b.D ? b.D() : b.call(l)
  }
  for(var d = B.a(a, 0), f = 1;;) {
    if(f < c) {
      d = b.a ? b.a(d, B.a(a, f)) : b.call(l, d, B.a(a, f)), f += 1
    }else {
      return d
    }
  }
}
function Hb(a, b, c) {
  for(var d = Ca(a), f = 0;;) {
    if(f < d) {
      c = b.a ? b.a(c, B.a(a, f)) : b.call(l, c, B.a(a, f)), f += 1
    }else {
      return c
    }
  }
}
function Ib(a, b, c, d) {
  for(var f = Ca(a);;) {
    if(d < f) {
      c = b.a ? b.a(c, B.a(a, d)) : b.call(l, c, B.a(a, d)), d += 1
    }else {
      return c
    }
  }
}
Fb = function(a, b, c, d) {
  switch(arguments.length) {
    case 2:
      return Gb.call(this, a, b);
    case 3:
      return Hb.call(this, a, b, c);
    case 4:
      return Ib.call(this, a, b, c, d)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Fb.a = Gb;
Fb.c = Hb;
Fb.p = Ib;
Eb = Fb;
var Jb, Kb = l;
function Lb(a, b) {
  var c = a.length;
  if(0 === a.length) {
    return b.D ? b.D() : b.call(l)
  }
  for(var d = a[0], f = 1;;) {
    if(f < c) {
      d = b.a ? b.a(d, a[f]) : b.call(l, d, a[f]), f += 1
    }else {
      return d
    }
  }
}
function Mb(a, b, c) {
  for(var d = a.length, f = 0;;) {
    if(f < d) {
      c = b.a ? b.a(c, a[f]) : b.call(l, c, a[f]), f += 1
    }else {
      return c
    }
  }
}
function Nb(a, b, c, d) {
  for(var f = a.length;;) {
    if(d < f) {
      c = b.a ? b.a(c, a[d]) : b.call(l, c, a[d]), d += 1
    }else {
      return c
    }
  }
}
Kb = function(a, b, c, d) {
  switch(arguments.length) {
    case 2:
      return Lb.call(this, a, b);
    case 3:
      return Mb.call(this, a, b, c);
    case 4:
      return Nb.call(this, a, b, c, d)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Kb.a = Lb;
Kb.c = Mb;
Kb.p = Nb;
Jb = Kb;
function Ob(a) {
  if(a) {
    var b = a.j & 2, a = (b ? b : a.rb) ? k : a.j ? p : z(Ba, a)
  }else {
    a = z(Ba, a)
  }
  return a
}
function Pb(a) {
  if(a) {
    var b = a.j & 16, a = (b ? b : a.Va) ? k : a.j ? p : z(Ea, a)
  }else {
    a = z(Ea, a)
  }
  return a
}
function vb(a, b) {
  this.b = a;
  this.o = b;
  this.r = 0;
  this.j = 166199550
}
w = vb.prototype;
w.G = function(a) {
  return Qb.e ? Qb.e(a) : Qb.call(l, a)
};
w.ea = function() {
  return this.o + 1 < this.b.length ? new vb(this.b, this.o + 1) : l
};
w.A = function(a, b) {
  return S.a ? S.a(b, a) : S.call(l, b, a)
};
w.toString = function() {
  return ub(this)
};
w.na = function(a, b) {
  return Jb.p(this.b, b, this.b[this.o], this.o + 1)
};
w.oa = function(a, b, c) {
  return Jb.p(this.b, b, c, this.o)
};
w.z = aa();
w.B = function() {
  return this.b.length - this.o
};
w.O = function() {
  return this.b[this.o]
};
w.P = function() {
  return this.o + 1 < this.b.length ? new vb(this.b, this.o + 1) : Rb.D ? Rb.D() : Rb.call(l)
};
w.C = function(a, b) {
  return Sb.a ? Sb.a(a, b) : Sb.call(l, a, b)
};
w.M = function(a, b) {
  var c = b + this.o;
  return c < this.b.length ? this.b[c] : l
};
w.V = function(a, b, c) {
  a = b + this.o;
  return a < this.b.length ? this.b[a] : c
};
var Tb, Ub = l;
function Vb(a) {
  return Ub.a(a, 0)
}
function Wb(a, b) {
  return b < a.length ? new vb(a, b) : l
}
Ub = function(a, b) {
  switch(arguments.length) {
    case 1:
      return Vb.call(this, a);
    case 2:
      return Wb.call(this, a, b)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Ub.e = Vb;
Ub.a = Wb;
Tb = Ub;
var R, Xb = l;
function Yb(a) {
  return Tb.a(a, 0)
}
function Zb(a, b) {
  return Tb.a(a, b)
}
Xb = function(a, b) {
  switch(arguments.length) {
    case 1:
      return Yb.call(this, a);
    case 2:
      return Zb.call(this, a, b)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Xb.e = Yb;
Xb.a = Zb;
R = Xb;
Ba.array = k;
Ca.array = function(a) {
  return a.length
};
eb._ = function(a, b) {
  return a === b
};
var $b, ac = l;
function dc(a, b) {
  return a != l ? Da(a, b) : Rb.e ? Rb.e(b) : Rb.call(l, b)
}
function ec(a, b, c) {
  for(;;) {
    if(y(c)) {
      a = ac.a(a, b), b = J(c), c = L(c)
    }else {
      return ac.a(a, b)
    }
  }
}
function fc(a, b, c) {
  var d = l;
  2 < arguments.length && (d = R(Array.prototype.slice.call(arguments, 2), 0));
  return ec.call(this, a, b, d)
}
fc.n = 2;
fc.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = K(a);
  return ec(b, c, a)
};
fc.g = ec;
ac = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return dc.call(this, a, b);
    default:
      return fc.g(a, b, R(arguments, 2))
  }
  e(Error("Invalid arity: " + arguments.length))
};
ac.n = 2;
ac.k = fc.k;
ac.a = dc;
ac.g = fc.g;
$b = ac;
function T(a) {
  if(Ob(a)) {
    a = Ca(a)
  }else {
    a: {
      for(var a = H(a), b = 0;;) {
        if(Ob(a)) {
          a = b + Ca(a);
          break a
        }
        a = L(a);
        b += 1
      }
      a = g
    }
  }
  return a
}
var gc, hc = l;
function ic(a, b) {
  for(;;) {
    a == l && e(Error("Index out of bounds"));
    if(0 === b) {
      if(H(a)) {
        return J(a)
      }
      e(Error("Index out of bounds"))
    }
    if(Pb(a)) {
      return B.a(a, b)
    }
    if(H(a)) {
      var c = L(a), d = b - 1, a = c, b = d
    }else {
      e(Error("Index out of bounds"))
    }
  }
}
function jc(a, b, c) {
  for(;;) {
    if(a == l) {
      return c
    }
    if(0 === b) {
      return H(a) ? J(a) : c
    }
    if(Pb(a)) {
      return B.c(a, b, c)
    }
    if(H(a)) {
      a = L(a), b -= 1
    }else {
      return c
    }
  }
}
hc = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return ic.call(this, a, b);
    case 3:
      return jc.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
hc.a = ic;
hc.c = jc;
gc = hc;
var U, kc = l;
function lc(a, b) {
  var c;
  if(a == l) {
    c = l
  }else {
    if(c = a) {
      c = (c = a.j & 16) ? c : a.Va
    }
    c = c ? a.M(a, Math.floor(b)) : a instanceof Array ? b < a.length ? a[b] : l : wa(a) ? b < a.length ? a[b] : l : gc.a(a, Math.floor(b))
  }
  return c
}
function mc(a, b, c) {
  if(a != l) {
    var d;
    if(d = a) {
      d = (d = a.j & 16) ? d : a.Va
    }
    a = d ? a.V(a, Math.floor(b), c) : a instanceof Array ? b < a.length ? a[b] : c : wa(a) ? b < a.length ? a[b] : c : gc.c(a, Math.floor(b), c)
  }else {
    a = c
  }
  return a
}
kc = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return lc.call(this, a, b);
    case 3:
      return mc.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
kc.a = lc;
kc.c = mc;
U = kc;
var nc, oc = l;
function pc(a, b) {
  var c;
  if(a == l) {
    c = l
  }else {
    if(c = a) {
      c = (c = a.j & 256) ? c : a.eb
    }
    c = c ? a.F(a, b) : a instanceof Array ? b < a.length ? a[b] : l : wa(a) ? b < a.length ? a[b] : l : z(Ia, a) ? Ja.a(a, b) : l
  }
  return c
}
function qc(a, b, c) {
  if(a != l) {
    var d;
    if(d = a) {
      d = (d = a.j & 256) ? d : a.eb
    }
    a = d ? a.t(a, b, c) : a instanceof Array ? b < a.length ? a[b] : c : wa(a) ? b < a.length ? a[b] : c : z(Ia, a) ? Ja.c(a, b, c) : c
  }else {
    a = c
  }
  return a
}
oc = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return pc.call(this, a, b);
    case 3:
      return qc.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
oc.a = pc;
oc.c = qc;
nc = oc;
var rc, sc = l;
function tc(a, b, c) {
  return a != l ? Oa(a, b, c) : uc.a ? uc.a(b, c) : uc.call(l, b, c)
}
function vc(a, b, c, d) {
  for(;;) {
    if(a = sc.c(a, b, c), y(d)) {
      b = J(d), c = J(L(d)), d = L(L(d))
    }else {
      return a
    }
  }
}
function wc(a, b, c, d) {
  var f = l;
  3 < arguments.length && (f = R(Array.prototype.slice.call(arguments, 3), 0));
  return vc.call(this, a, b, c, f)
}
wc.n = 3;
wc.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = L(a), d = J(a), a = K(a);
  return vc(b, c, d, a)
};
wc.g = vc;
sc = function(a, b, c, d) {
  switch(arguments.length) {
    case 3:
      return tc.call(this, a, b, c);
    default:
      return wc.g(a, b, c, R(arguments, 3))
  }
  e(Error("Invalid arity: " + arguments.length))
};
sc.n = 3;
sc.k = wc.k;
sc.c = tc;
sc.g = wc.g;
rc = sc;
function xc(a) {
  var b = "function" == x(a);
  return b ? b : a ? y(y(l) ? l : a.ab) ? k : a.Ab ? p : z(Aa, a) : z(Aa, a)
}
var Ac = function yc(b, c) {
  var d;
  if(d = xc(b)) {
    d = b ? ((d = b.j & 262144) ? d : b.zb) || (b.j ? 0 : z(Ya, b)) : z(Ya, b), d = !d
  }
  if(d) {
    if(g === la) {
      la = {};
      la = function(b, c, d, f) {
        this.m = b;
        this.Ra = c;
        this.pb = d;
        this.mb = f;
        this.r = 0;
        this.j = 393217
      };
      la.$a = k;
      la.lb = "cljs.core/t3810";
      la.kb = function(b) {
        return F(b, "cljs.core/t3810")
      };
      var f = function(b, c) {
        return zc.a ? zc.a(b.Ra, c) : zc.call(l, b.Ra, c)
      };
      d = function(b, c) {
        var b = this, d = l;
        1 < arguments.length && (d = R(Array.prototype.slice.call(arguments, 1), 0));
        return f.call(this, b, d)
      };
      d.n = 1;
      d.k = function(b) {
        var c = J(b), b = K(b);
        return f(c, b)
      };
      d.g = f;
      la.prototype.call = d;
      la.prototype.apply = function(b, c) {
        b = this;
        return b.call.apply(b, [b].concat(c.slice()))
      };
      la.prototype.ab = k;
      la.prototype.K = q("mb");
      la.prototype.I = function(b, c) {
        return new la(this.m, this.Ra, this.pb, c)
      }
    }
    d = new la(c, b, yc, l);
    d = yc(d, c)
  }else {
    d = Za(b, c)
  }
  return d
};
function Bc(a) {
  var b;
  b = a ? ((b = a.j & 131072) ? b : a.gb) || (a.j ? 0 : z(Wa, a)) : z(Wa, a);
  return b ? Xa(a) : l
}
var Cc = {}, Dc = 0, V, Fc = l;
function Gc(a) {
  return Fc.a(a, k)
}
function Hc(a, b) {
  var c;
  ((c = ba(a)) ? b : c) ? (255 < Dc && (Cc = {}, Dc = 0), c = Cc[a], "number" !== typeof c && (c = ea(a), Cc[a] = c, Dc += 1)) : c = fb(a);
  return c
}
Fc = function(a, b) {
  switch(arguments.length) {
    case 1:
      return Gc.call(this, a);
    case 2:
      return Hc.call(this, a, b)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Fc.e = Gc;
Fc.a = Hc;
V = Fc;
function Ic(a) {
  if(a) {
    var b = a.j & 16384, a = (b ? b : a.yb) ? k : a.j ? p : z(Va, a)
  }else {
    a = z(Va, a)
  }
  return a
}
function Jc(a) {
  var b = a instanceof Kc;
  return b ? b : a instanceof Lc
}
function Mc(a, b, c, d, f) {
  for(;0 !== f;) {
    c[d] = a[b], d += 1, f -= 1, b += 1
  }
}
var Nc = {};
function Oc(a) {
  var b = ba(a);
  return b ? "\ufdd0" === a.charAt(0) : b
}
var Pc, Qc = l;
function Rc(a, b) {
  return!O.a(a, b)
}
function Sc(a, b, c) {
  if(O.a(a, b)) {
    return p
  }
  a: {
    if(a = [b, l, a, l], b = a.length, b / 2 <= Tc) {
      a = new Uc(l, ta.a ? ta.a(a, k) : ta.call(l, a, k), l)
    }else {
      for(var d = 0, f = mb(Vc);;) {
        if(d < b) {
          var h = d + 2, f = nb(f, a[d]), d = h
        }else {
          a = ob(f);
          break a
        }
      }
      a = g
    }
  }
  for(b = c;;) {
    if(d = J(b), c = L(b), y(b)) {
      if(nc.c(a, d, Nc) !== Nc) {
        return p
      }
      a = $b.a(a, d);
      b = c
    }else {
      return k
    }
  }
}
function Wc(a, b, c) {
  var d = l;
  2 < arguments.length && (d = R(Array.prototype.slice.call(arguments, 2), 0));
  return Sc.call(this, a, b, d)
}
Wc.n = 2;
Wc.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = K(a);
  return Sc(b, c, a)
};
Wc.g = Sc;
Qc = function(a, b, c) {
  switch(arguments.length) {
    case 1:
      return k;
    case 2:
      return Rc.call(this, a, b);
    default:
      return Wc.g(a, b, R(arguments, 2))
  }
  e(Error("Invalid arity: " + arguments.length))
};
Qc.n = 2;
Qc.k = Wc.k;
Qc.e = t(k);
Qc.a = Rc;
Qc.g = Wc.g;
Pc = Qc;
function Xc(a, b) {
  if(a === b) {
    return 0
  }
  if(a == l) {
    return-1
  }
  if(b == l) {
    return 1
  }
  if((a == l ? l : a.constructor) === (b == l ? l : b.constructor)) {
    var c;
    if(c = a) {
      c = (c = a.r & 2048) ? c : a.bb
    }
    return c ? a.cb(a, b) : ga(a, b)
  }
  e(Error("compare on non-nil objects of different types"))
}
var Yc, Zc = l;
function $c(a, b) {
  var c = T(a), d = T(b);
  return c < d ? -1 : c > d ? 1 : Zc.p(a, b, c, 0)
}
function ad(a, b, c, d) {
  for(;;) {
    var f = Xc(U.a(a, d), U.a(b, d)), h = 0 === f;
    if(h ? d + 1 < c : h) {
      d += 1
    }else {
      return f
    }
  }
}
Zc = function(a, b, c, d) {
  switch(arguments.length) {
    case 2:
      return $c.call(this, a, b);
    case 4:
      return ad.call(this, a, b, c, d)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Zc.a = $c;
Zc.p = ad;
Yc = Zc;
var bd, cd = l;
function dd(a) {
  return cd.a(Xc, a)
}
function ed(a, b) {
  if(H(b)) {
    var c = fd.e ? fd.e(b) : fd.call(l, b), d;
    d = O.a(a, Xc) ? Xc : function(b, c) {
      var d = a.a ? a.a(b, c) : a.call(l, b, c);
      return"number" === typeof d ? d : y(d) ? -1 : y(a.a ? a.a(c, b) : a.call(l, c, b)) ? 1 : 0
    };
    for(var f = 0;f < c.length;f++) {
      c[f] = {index:f, value:c[f]}
    }
    var h = d || ga;
    fa.sort.call(c, function(a, b) {
      return h(a.value, b.value) || a.index - b.index
    } || ga);
    for(f = 0;f < c.length;f++) {
      c[f] = c[f].value
    }
    return H(c)
  }
  return wb
}
cd = function(a, b) {
  switch(arguments.length) {
    case 1:
      return dd.call(this, a);
    case 2:
      return ed.call(this, a, b)
  }
  e(Error("Invalid arity: " + arguments.length))
};
cd.e = dd;
cd.a = ed;
bd = cd;
var gd, hd = l;
function id(a, b) {
  var c = H(b);
  return c ? jd.c ? jd.c(a, J(c), L(c)) : jd.call(l, a, J(c), L(c)) : a.D ? a.D() : a.call(l)
}
function ld(a, b, c) {
  for(c = H(c);;) {
    if(c) {
      b = a.a ? a.a(b, J(c)) : a.call(l, b, J(c)), c = L(c)
    }else {
      return b
    }
  }
}
hd = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return id.call(this, a, b);
    case 3:
      return ld.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
hd.a = id;
hd.c = ld;
gd = hd;
var jd, md = l;
function nd(a, b) {
  var c;
  if(c = b) {
    c = (c = b.j & 524288) ? c : b.ib
  }
  return c ? b.na(b, a) : b instanceof Array ? Jb.a(b, a) : wa(b) ? Jb.a(b, a) : z($a, b) ? ab.a(b, a) : gd.a(a, b)
}
function od(a, b, c) {
  var d;
  if(d = c) {
    d = (d = c.j & 524288) ? d : c.ib
  }
  return d ? c.oa(c, a, b) : c instanceof Array ? Jb.c(c, a, b) : wa(c) ? Jb.c(c, a, b) : z($a, c) ? ab.c(c, a, b) : gd.c(a, b, c)
}
md = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return nd.call(this, a, b);
    case 3:
      return od.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
md.a = nd;
md.c = od;
jd = md;
var pd, qd = l;
function rd(a, b, c) {
  for(;;) {
    if(a <= b) {
      if(L(c)) {
        a = b, b = J(c), c = L(c)
      }else {
        return b <= J(c)
      }
    }else {
      return p
    }
  }
}
function sd(a, b, c) {
  var d = l;
  2 < arguments.length && (d = R(Array.prototype.slice.call(arguments, 2), 0));
  return rd.call(this, a, b, d)
}
sd.n = 2;
sd.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = K(a);
  return rd(b, c, a)
};
sd.g = rd;
qd = function(a, b, c) {
  switch(arguments.length) {
    case 1:
      return k;
    case 2:
      return a <= b;
    default:
      return sd.g(a, b, R(arguments, 2))
  }
  e(Error("Invalid arity: " + arguments.length))
};
qd.n = 2;
qd.k = sd.k;
qd.e = t(k);
qd.a = function(a, b) {
  return a <= b
};
qd.g = sd.g;
pd = qd;
function td(a) {
  return 0 <= (a - a % 2) / 2 ? Math.floor.e ? Math.floor.e((a - a % 2) / 2) : Math.floor.call(l, (a - a % 2) / 2) : Math.ceil.e ? Math.ceil.e((a - a % 2) / 2) : Math.ceil.call(l, (a - a % 2) / 2)
}
function ud(a) {
  a -= a >> 1 & 1431655765;
  a = (a & 858993459) + (a >> 2 & 858993459);
  return 16843009 * (a + (a >> 4) & 252645135) >> 24
}
var vd, wd = l;
function xd(a) {
  return a == l ? "" : a.toString()
}
function yd(a, b) {
  return function(a, b) {
    for(;;) {
      if(y(b)) {
        var f = a.append(wd.e(J(b))), h = L(b), a = f, b = h
      }else {
        return wd.e(a)
      }
    }
  }.call(l, new ka(wd.e(a)), b)
}
function zd(a, b) {
  var c = l;
  1 < arguments.length && (c = R(Array.prototype.slice.call(arguments, 1), 0));
  return yd.call(this, a, c)
}
zd.n = 1;
zd.k = function(a) {
  var b = J(a), a = K(a);
  return yd(b, a)
};
zd.g = yd;
wd = function(a, b) {
  switch(arguments.length) {
    case 0:
      return"";
    case 1:
      return xd.call(this, a);
    default:
      return zd.g(a, R(arguments, 1))
  }
  e(Error("Invalid arity: " + arguments.length))
};
wd.n = 1;
wd.k = zd.k;
wd.D = t("");
wd.e = xd;
wd.g = zd.g;
vd = wd;
var G, Ad = l;
function Bd(a) {
  return Oc(a) ? vd.g(":", R([a.substring(2, a.length)], 0)) : a == l ? "" : a.toString()
}
function Cd(a, b) {
  return function(a, b) {
    for(;;) {
      if(y(b)) {
        var f = a.append(Ad.e(J(b))), h = L(b), a = f, b = h
      }else {
        return vd.e(a)
      }
    }
  }.call(l, new ka(Ad.e(a)), b)
}
function Dd(a, b) {
  var c = l;
  1 < arguments.length && (c = R(Array.prototype.slice.call(arguments, 1), 0));
  return Cd.call(this, a, c)
}
Dd.n = 1;
Dd.k = function(a) {
  var b = J(a), a = K(a);
  return Cd(b, a)
};
Dd.g = Cd;
Ad = function(a, b) {
  switch(arguments.length) {
    case 0:
      return"";
    case 1:
      return Bd.call(this, a);
    default:
      return Dd.g(a, R(arguments, 1))
  }
  e(Error("Invalid arity: " + arguments.length))
};
Ad.n = 1;
Ad.k = Dd.k;
Ad.D = t("");
Ad.e = Bd;
Ad.g = Dd.g;
G = Ad;
var Ed, Fd = l, Fd = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return a.substring(b);
    case 3:
      return a.substring(b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Fd.a = function(a, b) {
  return a.substring(b)
};
Fd.c = function(a, b, c) {
  return a.substring(b, c)
};
Ed = Fd;
function Sb(a, b) {
  var c;
  c = b ? ((c = b.j & 16777216) ? c : b.wb) || (b.j ? 0 : z(kb, b)) : z(kb, b);
  if(c) {
    a: {
      c = H(a);
      for(var d = H(b);;) {
        if(c == l) {
          c = d == l;
          break a
        }
        if(d != l && O.a(J(c), J(d))) {
          c = L(c), d = L(d)
        }else {
          c = p;
          break a
        }
      }
      c = g
    }
  }else {
    c = l
  }
  return y(c) ? k : p
}
function Gd(a, b) {
  return a ^ b + 2654435769 + (a << 6) + (a >> 2)
}
function Qb(a) {
  return jd.c(function(a, c) {
    return Gd(a, V.a(c, p))
  }, V.a(J(a), p), L(a))
}
function Hd(a) {
  for(var b = 0, a = H(a);;) {
    if(a) {
      var c = J(a), b = (b + (V.e(Id.e ? Id.e(c) : Id.call(l, c)) ^ V.e(Jd.e ? Jd.e(c) : Jd.call(l, c)))) % 4503599627370496, a = L(a)
    }else {
      return b
    }
  }
}
function Kd(a, b, c, d, f) {
  this.m = a;
  this.wa = b;
  this.ca = c;
  this.count = d;
  this.l = f;
  this.r = 0;
  this.j = 65413358
}
w = Kd.prototype;
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Qb(a)
};
w.ea = function() {
  return 1 === this.count ? l : this.ca
};
w.A = function(a, b) {
  return new Kd(this.m, b, a, this.count + 1, l)
};
w.toString = function() {
  return ub(this)
};
w.z = aa();
w.B = q("count");
w.O = q("wa");
w.P = function() {
  return 1 === this.count ? wb : this.ca
};
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return new Kd(b, this.wa, this.ca, this.count, this.l)
};
w.K = q("m");
function Ld(a) {
  this.m = a;
  this.r = 0;
  this.j = 65413326
}
w = Ld.prototype;
w.G = t(0);
w.ea = t(l);
w.A = function(a, b) {
  return new Kd(this.m, b, l, 1, l)
};
w.toString = function() {
  return ub(this)
};
w.z = t(l);
w.B = t(0);
w.O = t(l);
w.P = function() {
  return wb
};
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return new Ld(b)
};
w.K = q("m");
var wb = new Ld(l), Rb;
function Md(a) {
  var b;
  if(a instanceof vb) {
    b = a.b
  }else {
    a: {
      for(b = [];;) {
        if(a != l) {
          b.push(a.O(a)), a = a.ea(a)
        }else {
          break a
        }
      }
      b = g
    }
  }
  for(var a = b.length, c = wb;;) {
    if(0 < a) {
      var d = a - 1, c = c.A(c, b[a - 1]), a = d
    }else {
      return c
    }
  }
}
function Nd(a) {
  var b = l;
  0 < arguments.length && (b = R(Array.prototype.slice.call(arguments, 0), 0));
  return Md.call(this, b)
}
Nd.n = 0;
Nd.k = function(a) {
  a = H(a);
  return Md(a)
};
Nd.g = Md;
Rb = Nd;
function Od(a, b, c, d) {
  this.m = a;
  this.wa = b;
  this.ca = c;
  this.l = d;
  this.r = 0;
  this.j = 65405164
}
w = Od.prototype;
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Qb(a)
};
w.ea = function() {
  return this.ca == l ? l : gb(this.ca)
};
w.A = function(a, b) {
  return new Od(l, b, a, this.l)
};
w.toString = function() {
  return ub(this)
};
w.z = aa();
w.O = q("wa");
w.P = function() {
  return this.ca == l ? wb : this.ca
};
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return new Od(b, this.wa, this.ca, this.l)
};
w.K = q("m");
function S(a, b) {
  var c = b == l;
  if(!c && (c = b)) {
    c = (c = b.j & 64) ? c : b.Ya
  }
  return c ? new Od(l, a, b, l) : new Od(l, a, H(b), l)
}
Ba.string = k;
Ca.string = function(a) {
  return a.length
};
fb.string = function(a) {
  return ea(a)
};
function Pd(a) {
  this.Qa = a;
  this.r = 0;
  this.j = 1
}
var Qd = l, Qd = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      var d;
      d = a;
      d = this;
      if(b == l) {
        d = l
      }else {
        var f = b.ja;
        d = f == l ? Ja.c(b, d.Qa, l) : f[d.Qa]
      }
      return d;
    case 3:
      return b == l ? c : Ja.c(b, this.Qa, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Pd.prototype.call = Qd;
Pd.prototype.apply = function(a, b) {
  a = this;
  return a.call.apply(a, [a].concat(b.slice()))
};
var Rd = l, Rd = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return nc.a(b, this.toString());
    case 3:
      return nc.c(b, this.toString(), c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
String.prototype.call = Rd;
String.prototype.apply = function(a, b) {
  return a.call.apply(a, [a].concat(b.slice()))
};
String.prototype.apply = function(a, b) {
  return 2 > b.length ? nc.a(b[0], a) : nc.c(b[0], a, b[1])
};
function Sd(a) {
  var b = a.x;
  if(a.Sa) {
    return b
  }
  a.x = b.D ? b.D() : b.call(l);
  a.Sa = k;
  return a.x
}
function W(a, b, c, d) {
  this.m = a;
  this.Sa = b;
  this.x = c;
  this.l = d;
  this.r = 0;
  this.j = 31850700
}
w = W.prototype;
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Qb(a)
};
w.ea = function(a) {
  return gb(a.P(a))
};
w.A = function(a, b) {
  return S(b, a)
};
w.toString = function() {
  return ub(this)
};
w.z = function(a) {
  return H(Sd(a))
};
w.O = function(a) {
  return J(Sd(a))
};
w.P = function(a) {
  return K(Sd(a))
};
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return new W(b, this.Sa, this.x, this.l)
};
w.K = q("m");
function Td(a, b) {
  this.Ba = a;
  this.end = b;
  this.r = 0;
  this.j = 2
}
Td.prototype.B = q("end");
Td.prototype.add = function(a) {
  this.Ba[this.end] = a;
  return this.end += 1
};
Td.prototype.T = function() {
  var a = new Ud(this.Ba, 0, this.end);
  this.Ba = l;
  return a
};
function Ud(a, b, c) {
  this.b = a;
  this.w = b;
  this.end = c;
  this.r = 0;
  this.j = 524306
}
w = Ud.prototype;
w.na = function(a, b) {
  return Jb.p(this.b, b, this.b[this.w], this.w + 1)
};
w.oa = function(a, b, c) {
  return Jb.p(this.b, b, c, this.w)
};
w.Ta = function() {
  this.w === this.end && e(Error("-drop-first of empty chunk"));
  return new Ud(this.b, this.w + 1, this.end)
};
w.M = function(a, b) {
  return this.b[this.w + b]
};
w.V = function(a, b, c) {
  return((a = 0 <= b) ? b < this.end - this.w : a) ? this.b[this.w + b] : c
};
w.B = function() {
  return this.end - this.w
};
var Vd, Wd = l;
function Xd(a) {
  return new Ud(a, 0, a.length)
}
function Yd(a, b) {
  return new Ud(a, b, a.length)
}
function Zd(a, b, c) {
  return new Ud(a, b, c)
}
Wd = function(a, b, c) {
  switch(arguments.length) {
    case 1:
      return Xd.call(this, a);
    case 2:
      return Yd.call(this, a, b);
    case 3:
      return Zd.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Wd.e = Xd;
Wd.a = Yd;
Wd.c = Zd;
Vd = Wd;
function Kc(a, b, c, d) {
  this.T = a;
  this.ha = b;
  this.m = c;
  this.l = d;
  this.j = 31850604;
  this.r = 1536
}
w = Kc.prototype;
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Qb(a)
};
w.A = function(a, b) {
  return S(b, a)
};
w.toString = function() {
  return ub(this)
};
w.z = aa();
w.O = function() {
  return B.a(this.T, 0)
};
w.P = function() {
  return 1 < Ca(this.T) ? new Kc(qb(this.T), this.ha, this.m, l) : this.ha == l ? wb : this.ha
};
w.Ua = function() {
  return this.ha == l ? l : this.ha
};
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return new Kc(this.T, this.ha, b, this.l)
};
w.K = q("m");
w.Ca = q("T");
w.za = function() {
  return this.ha == l ? wb : this.ha
};
function $d(a, b) {
  return 0 === Ca(a) ? b : new Kc(a, b, l, l)
}
function fd(a) {
  for(var b = [];;) {
    if(H(a)) {
      b.push(J(a)), a = L(a)
    }else {
      return b
    }
  }
}
function ae(a, b) {
  if(Ob(a)) {
    return T(a)
  }
  for(var c = a, d = b, f = 0;;) {
    var h;
    h = (h = 0 < d) ? H(c) : h;
    if(y(h)) {
      c = L(c), d -= 1, f += 1
    }else {
      return f
    }
  }
}
var ce = function be(b) {
  return b == l ? l : L(b) == l ? H(J(b)) : S(J(b), be(L(b)))
}, de, ee = l;
function fe() {
  return new W(l, p, t(l), l)
}
function ge(a) {
  return new W(l, p, function() {
    return a
  }, l)
}
function he(a, b) {
  return new W(l, p, function() {
    var c = H(a);
    return c ? Jc(c) ? $d(rb(c), ee.a(sb(c), b)) : S(J(c), ee.a(K(c), b)) : b
  }, l)
}
function ie(a, b, c) {
  return function f(a, b) {
    return new W(l, p, function() {
      var c = H(a);
      return c ? Jc(c) ? $d(rb(c), f(sb(c), b)) : S(J(c), f(K(c), b)) : y(b) ? f(J(b), L(b)) : l
    }, l)
  }(ee.a(a, b), c)
}
function je(a, b, c) {
  var d = l;
  2 < arguments.length && (d = R(Array.prototype.slice.call(arguments, 2), 0));
  return ie.call(this, a, b, d)
}
je.n = 2;
je.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = K(a);
  return ie(b, c, a)
};
je.g = ie;
ee = function(a, b, c) {
  switch(arguments.length) {
    case 0:
      return fe.call(this);
    case 1:
      return ge.call(this, a);
    case 2:
      return he.call(this, a, b);
    default:
      return je.g(a, b, R(arguments, 2))
  }
  e(Error("Invalid arity: " + arguments.length))
};
ee.n = 2;
ee.k = je.k;
ee.D = fe;
ee.e = ge;
ee.a = he;
ee.g = je.g;
de = ee;
var ke, le = l;
function me(a, b, c) {
  return S(a, S(b, c))
}
function ne(a, b, c, d) {
  return S(a, S(b, S(c, d)))
}
function oe(a, b, c, d, f) {
  return S(a, S(b, S(c, S(d, ce(f)))))
}
function pe(a, b, c, d, f) {
  var h = l;
  4 < arguments.length && (h = R(Array.prototype.slice.call(arguments, 4), 0));
  return oe.call(this, a, b, c, d, h)
}
pe.n = 4;
pe.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = L(a), d = J(a), a = L(a), f = J(a), a = K(a);
  return oe(b, c, d, f, a)
};
pe.g = oe;
le = function(a, b, c, d, f) {
  switch(arguments.length) {
    case 1:
      return H(a);
    case 2:
      return S(a, b);
    case 3:
      return me.call(this, a, b, c);
    case 4:
      return ne.call(this, a, b, c, d);
    default:
      return pe.g(a, b, c, d, R(arguments, 4))
  }
  e(Error("Invalid arity: " + arguments.length))
};
le.n = 4;
le.k = pe.k;
le.e = function(a) {
  return H(a)
};
le.a = function(a, b) {
  return S(a, b)
};
le.c = me;
le.p = ne;
le.g = pe.g;
ke = le;
function qe(a) {
  return ob(a)
}
function re(a, b, c) {
  var d = H(c);
  if(0 === b) {
    return a.D ? a.D() : a.call(l)
  }
  var c = C(d), f = D(d);
  if(1 === b) {
    return a.e ? a.e(c) : a.e ? a.e(c) : a.call(l, c)
  }
  var d = C(f), h = D(f);
  if(2 === b) {
    return a.a ? a.a(c, d) : a.a ? a.a(c, d) : a.call(l, c, d)
  }
  var f = C(h), i = D(h);
  if(3 === b) {
    return a.c ? a.c(c, d, f) : a.c ? a.c(c, d, f) : a.call(l, c, d, f)
  }
  var h = C(i), j = D(i);
  if(4 === b) {
    return a.p ? a.p(c, d, f, h) : a.p ? a.p(c, d, f, h) : a.call(l, c, d, f, h)
  }
  i = C(j);
  j = D(j);
  if(5 === b) {
    return a.U ? a.U(c, d, f, h, i) : a.U ? a.U(c, d, f, h, i) : a.call(l, c, d, f, h, i)
  }
  var a = C(j), m = D(j);
  if(6 === b) {
    return a.aa ? a.aa(c, d, f, h, i, a) : a.aa ? a.aa(c, d, f, h, i, a) : a.call(l, c, d, f, h, i, a)
  }
  var j = C(m), n = D(m);
  if(7 === b) {
    return a.ma ? a.ma(c, d, f, h, i, a, j) : a.ma ? a.ma(c, d, f, h, i, a, j) : a.call(l, c, d, f, h, i, a, j)
  }
  var m = C(n), s = D(n);
  if(8 === b) {
    return a.Oa ? a.Oa(c, d, f, h, i, a, j, m) : a.Oa ? a.Oa(c, d, f, h, i, a, j, m) : a.call(l, c, d, f, h, i, a, j, m)
  }
  var n = C(s), r = D(s);
  if(9 === b) {
    return a.Pa ? a.Pa(c, d, f, h, i, a, j, m, n) : a.Pa ? a.Pa(c, d, f, h, i, a, j, m, n) : a.call(l, c, d, f, h, i, a, j, m, n)
  }
  var s = C(r), v = D(r);
  if(10 === b) {
    return a.Da ? a.Da(c, d, f, h, i, a, j, m, n, s) : a.Da ? a.Da(c, d, f, h, i, a, j, m, n, s) : a.call(l, c, d, f, h, i, a, j, m, n, s)
  }
  var r = C(v), u = D(v);
  if(11 === b) {
    return a.Ea ? a.Ea(c, d, f, h, i, a, j, m, n, s, r) : a.Ea ? a.Ea(c, d, f, h, i, a, j, m, n, s, r) : a.call(l, c, d, f, h, i, a, j, m, n, s, r)
  }
  var v = C(u), E = D(u);
  if(12 === b) {
    return a.Fa ? a.Fa(c, d, f, h, i, a, j, m, n, s, r, v) : a.Fa ? a.Fa(c, d, f, h, i, a, j, m, n, s, r, v) : a.call(l, c, d, f, h, i, a, j, m, n, s, r, v)
  }
  var u = C(E), I = D(E);
  if(13 === b) {
    return a.Ga ? a.Ga(c, d, f, h, i, a, j, m, n, s, r, v, u) : a.Ga ? a.Ga(c, d, f, h, i, a, j, m, n, s, r, v, u) : a.call(l, c, d, f, h, i, a, j, m, n, s, r, v, u)
  }
  var E = C(I), M = D(I);
  if(14 === b) {
    return a.Ha ? a.Ha(c, d, f, h, i, a, j, m, n, s, r, v, u, E) : a.Ha ? a.Ha(c, d, f, h, i, a, j, m, n, s, r, v, u, E) : a.call(l, c, d, f, h, i, a, j, m, n, s, r, v, u, E)
  }
  var I = C(M), P = D(M);
  if(15 === b) {
    return a.Ia ? a.Ia(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I) : a.Ia ? a.Ia(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I) : a.call(l, c, d, f, h, i, a, j, m, n, s, r, v, u, E, I)
  }
  var M = C(P), N = D(P);
  if(16 === b) {
    return a.Ja ? a.Ja(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M) : a.Ja ? a.Ja(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M) : a.call(l, c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M)
  }
  var P = C(N), ha = D(N);
  if(17 === b) {
    return a.Ka ? a.Ka(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P) : a.Ka ? a.Ka(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P) : a.call(l, c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P)
  }
  var N = C(ha), ua = D(ha);
  if(18 === b) {
    return a.La ? a.La(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P, N) : a.La ? a.La(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P, N) : a.call(l, c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P, N)
  }
  ha = C(ua);
  ua = D(ua);
  if(19 === b) {
    return a.Ma ? a.Ma(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P, N, ha) : a.Ma ? a.Ma(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P, N, ha) : a.call(l, c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P, N, ha)
  }
  var na = C(ua);
  D(ua);
  if(20 === b) {
    return a.Na ? a.Na(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P, N, ha, na) : a.Na ? a.Na(c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P, N, ha, na) : a.call(l, c, d, f, h, i, a, j, m, n, s, r, v, u, E, I, M, P, N, ha, na)
  }
  e(Error("Only up to 20 arguments supported on functions"))
}
var zc, ue = l;
function ve(a, b) {
  var c = a.n;
  if(a.k) {
    var d = ae(b, c + 1);
    return d <= c ? re(a, d, b) : a.k(b)
  }
  return a.apply(a, fd(b))
}
function we(a, b, c) {
  b = ke.a(b, c);
  c = a.n;
  if(a.k) {
    var d = ae(b, c + 1);
    return d <= c ? re(a, d, b) : a.k(b)
  }
  return a.apply(a, fd(b))
}
function xe(a, b, c, d) {
  b = ke.c(b, c, d);
  c = a.n;
  return a.k ? (d = ae(b, c + 1), d <= c ? re(a, d, b) : a.k(b)) : a.apply(a, fd(b))
}
function ye(a, b, c, d, f) {
  b = ke.p(b, c, d, f);
  c = a.n;
  return a.k ? (d = ae(b, c + 1), d <= c ? re(a, d, b) : a.k(b)) : a.apply(a, fd(b))
}
function ze(a, b, c, d, f, h) {
  b = S(b, S(c, S(d, S(f, ce(h)))));
  c = a.n;
  return a.k ? (d = ae(b, c + 1), d <= c ? re(a, d, b) : a.k(b)) : a.apply(a, fd(b))
}
function Ae(a, b, c, d, f, h) {
  var i = l;
  5 < arguments.length && (i = R(Array.prototype.slice.call(arguments, 5), 0));
  return ze.call(this, a, b, c, d, f, i)
}
Ae.n = 5;
Ae.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = L(a), d = J(a), a = L(a), f = J(a), a = L(a), h = J(a), a = K(a);
  return ze(b, c, d, f, h, a)
};
Ae.g = ze;
ue = function(a, b, c, d, f, h) {
  switch(arguments.length) {
    case 2:
      return ve.call(this, a, b);
    case 3:
      return we.call(this, a, b, c);
    case 4:
      return xe.call(this, a, b, c, d);
    case 5:
      return ye.call(this, a, b, c, d, f);
    default:
      return Ae.g(a, b, c, d, f, R(arguments, 5))
  }
  e(Error("Invalid arity: " + arguments.length))
};
ue.n = 5;
ue.k = Ae.k;
ue.a = ve;
ue.c = we;
ue.p = xe;
ue.U = ye;
ue.g = Ae.g;
zc = ue;
function Be(a, b) {
  for(;;) {
    if(H(b) == l) {
      return k
    }
    if(y(a.e ? a.e(J(b)) : a.call(l, J(b)))) {
      var c = a, d = L(b), a = c, b = d
    }else {
      return p
    }
  }
}
function Ce(a) {
  return a
}
var De, Ee = l;
function Fe(a, b) {
  function c(a) {
    var b = l;
    0 < arguments.length && (b = R(Array.prototype.slice.call(arguments, 0), 0));
    return d.call(this, b)
  }
  function d(c) {
    return zc.c(a, b, c)
  }
  c.n = 0;
  c.k = function(a) {
    a = H(a);
    return d(a)
  };
  c.g = d;
  return c
}
function Ge(a, b, c) {
  function d(a) {
    var b = l;
    0 < arguments.length && (b = R(Array.prototype.slice.call(arguments, 0), 0));
    return f.call(this, b)
  }
  function f(d) {
    return zc.p(a, b, c, d)
  }
  d.n = 0;
  d.k = function(a) {
    a = H(a);
    return f(a)
  };
  d.g = f;
  return d
}
function He(a, b, c, d) {
  function f(a) {
    var b = l;
    0 < arguments.length && (b = R(Array.prototype.slice.call(arguments, 0), 0));
    return h.call(this, b)
  }
  function h(f) {
    return zc.U(a, b, c, d, f)
  }
  f.n = 0;
  f.k = function(a) {
    a = H(a);
    return h(a)
  };
  f.g = h;
  return f
}
function Ie(a, b, c, d, f) {
  function h(a) {
    var b = l;
    0 < arguments.length && (b = R(Array.prototype.slice.call(arguments, 0), 0));
    return i.call(this, b)
  }
  function i(h) {
    return zc.U(a, b, c, d, de.a(f, h))
  }
  h.n = 0;
  h.k = function(a) {
    a = H(a);
    return i(a)
  };
  h.g = i;
  return h
}
function Je(a, b, c, d, f) {
  var h = l;
  4 < arguments.length && (h = R(Array.prototype.slice.call(arguments, 4), 0));
  return Ie.call(this, a, b, c, d, h)
}
Je.n = 4;
Je.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = L(a), d = J(a), a = L(a), f = J(a), a = K(a);
  return Ie(b, c, d, f, a)
};
Je.g = Ie;
Ee = function(a, b, c, d, f) {
  switch(arguments.length) {
    case 2:
      return Fe.call(this, a, b);
    case 3:
      return Ge.call(this, a, b, c);
    case 4:
      return He.call(this, a, b, c, d);
    default:
      return Je.g(a, b, c, d, R(arguments, 4))
  }
  e(Error("Invalid arity: " + arguments.length))
};
Ee.n = 4;
Ee.k = Je.k;
Ee.a = Fe;
Ee.c = Ge;
Ee.p = He;
Ee.g = Je.g;
De = Ee;
var Ke, Le = l;
function Me(a, b) {
  return new W(l, p, function() {
    var c = H(b);
    if(c) {
      if(Jc(c)) {
        for(var d = rb(c), f = T(d), h = new Td(Array(f), 0), i = 0;;) {
          if(i < f) {
            var j = a.e ? a.e(B.a(d, i)) : a.call(l, B.a(d, i));
            h.add(j);
            i += 1
          }else {
            break
          }
        }
        return $d(h.T(), Le.a(a, sb(c)))
      }
      return S(a.e ? a.e(J(c)) : a.call(l, J(c)), Le.a(a, K(c)))
    }
    return l
  }, l)
}
function Ne(a, b, c) {
  return new W(l, p, function() {
    var d = H(b), f = H(c);
    return(d ? f : d) ? S(a.a ? a.a(J(d), J(f)) : a.call(l, J(d), J(f)), Le.c(a, K(d), K(f))) : l
  }, l)
}
function Oe(a, b, c, d) {
  return new W(l, p, function() {
    var f = H(b), h = H(c), i = H(d);
    return(f ? h ? i : h : f) ? S(a.c ? a.c(J(f), J(h), J(i)) : a.call(l, J(f), J(h), J(i)), Le.p(a, K(f), K(h), K(i))) : l
  }, l)
}
function Pe(a, b, c, d, f) {
  return Le.a(function(b) {
    return zc.a(a, b)
  }, function i(a) {
    return new W(l, p, function() {
      var b = Le.a(H, a);
      return Be(Ce, b) ? S(Le.a(J, b), i(Le.a(K, b))) : l
    }, l)
  }($b.g(f, d, R([c, b], 0))))
}
function Qe(a, b, c, d, f) {
  var h = l;
  4 < arguments.length && (h = R(Array.prototype.slice.call(arguments, 4), 0));
  return Pe.call(this, a, b, c, d, h)
}
Qe.n = 4;
Qe.k = function(a) {
  var b = J(a), a = L(a), c = J(a), a = L(a), d = J(a), a = L(a), f = J(a), a = K(a);
  return Pe(b, c, d, f, a)
};
Qe.g = Pe;
Le = function(a, b, c, d, f) {
  switch(arguments.length) {
    case 2:
      return Me.call(this, a, b);
    case 3:
      return Ne.call(this, a, b, c);
    case 4:
      return Oe.call(this, a, b, c, d);
    default:
      return Qe.g(a, b, c, d, R(arguments, 4))
  }
  e(Error("Invalid arity: " + arguments.length))
};
Le.n = 4;
Le.k = Qe.k;
Le.a = Me;
Le.c = Ne;
Le.p = Oe;
Le.g = Qe.g;
Ke = Le;
var Se = function Re(b, c) {
  return new W(l, p, function() {
    if(0 < b) {
      var d = H(c);
      return d ? S(J(d), Re(b - 1, K(d))) : l
    }
    return l
  }, l)
};
function Te(a, b) {
  return new W(l, p, function() {
    var c;
    a: {
      c = a;
      for(var d = b;;) {
        var d = H(d), f = 0 < c;
        if(y(f ? d : f)) {
          c -= 1, d = K(d)
        }else {
          c = d;
          break a
        }
      }
      c = g
    }
    return c
  }, l)
}
var Ue, Ve = l;
function We(a) {
  return new W(l, p, function() {
    return S(a, Ve.e(a))
  }, l)
}
function Xe(a, b) {
  return Se(a, Ve.e(b))
}
Ve = function(a, b) {
  switch(arguments.length) {
    case 1:
      return We.call(this, a);
    case 2:
      return Xe.call(this, a, b)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Ve.e = We;
Ve.a = Xe;
Ue = Ve;
function Ye(a, b) {
  var c;
  if(a != l) {
    if(c = a) {
      c = (c = a.r & 4) ? c : a.sb
    }
    c = c ? qe(jd.c(nb, mb(a), b)) : jd.c(Da, a, b)
  }else {
    c = jd.c($b, wb, b)
  }
  return c
}
var Ze, $e = l;
function af(a, b) {
  return $e.c(a, a, b)
}
function bf(a, b, c) {
  return new W(l, p, function() {
    var d = H(c);
    if(d) {
      var f = Se(a, d);
      return a === T(f) ? S(f, $e.c(a, b, Te(b, d))) : l
    }
    return l
  }, l)
}
function cf(a, b, c, d) {
  return new W(l, p, function() {
    var f = H(d);
    if(f) {
      var h = Se(a, f);
      return a === T(h) ? S(h, $e.p(a, b, c, Te(b, f))) : Rb.g(R([Se(a, de.a(h, c))], 0))
    }
    return l
  }, l)
}
$e = function(a, b, c, d) {
  switch(arguments.length) {
    case 2:
      return af.call(this, a, b);
    case 3:
      return bf.call(this, a, b, c);
    case 4:
      return cf.call(this, a, b, c, d)
  }
  e(Error("Invalid arity: " + arguments.length))
};
$e.a = af;
$e.c = bf;
$e.p = cf;
Ze = $e;
function df(a, b) {
  this.q = a;
  this.b = b
}
function ef(a) {
  a = a.h;
  return 32 > a ? 0 : a - 1 >>> 5 << 5
}
function ff(a, b, c) {
  for(;;) {
    if(0 === b) {
      return c
    }
    var d = new df(a, Array(32));
    d.b[0] = c;
    c = d;
    b -= 5
  }
}
var hf = function gf(b, c, d, f) {
  var h = new df(d.q, d.b.slice()), i = b.h - 1 >>> c & 31;
  5 === c ? h.b[i] = f : (d = d.b[i], b = d != l ? gf(b, c - 5, d, f) : ff(l, c - 5, f), h.b[i] = b);
  return h
};
function jf(a, b) {
  var c = 0 <= b;
  if(c ? b < a.h : c) {
    if(b >= ef(a)) {
      return a.J
    }
    for(var c = a.root, d = a.shift;;) {
      if(0 < d) {
        var f = d - 5, c = c.b[b >>> d & 31], d = f
      }else {
        return c.b
      }
    }
  }else {
    e(Error([G("No item "), G(b), G(" in vector of length "), G(a.h)].join("")))
  }
}
var lf = function kf(b, c, d, f, h) {
  var i = new df(d.q, d.b.slice());
  if(0 === c) {
    i.b[f & 31] = h
  }else {
    var j = f >>> c & 31, b = kf(b, c - 5, d.b[j], f, h);
    i.b[j] = b
  }
  return i
};
function mf(a, b, c, d, f, h) {
  this.m = a;
  this.h = b;
  this.shift = c;
  this.root = d;
  this.J = f;
  this.l = h;
  this.r = 4;
  this.j = 167668511
}
w = mf.prototype;
w.la = function() {
  return new nf(this.h, this.shift, of.e ? of.e(this.root) : of.call(l, this.root), pf.e ? pf.e(this.J) : pf.call(l, this.J))
};
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Qb(a)
};
w.F = function(a, b) {
  return a.V(a, b, l)
};
w.t = function(a, b, c) {
  return a.V(a, b, c)
};
w.da = function(a, b, c) {
  var d = 0 <= b;
  if(d ? b < this.h : d) {
    return ef(a) <= b ? (a = this.J.slice(), a[b & 31] = c, new mf(this.m, this.h, this.shift, this.root, a, l)) : new mf(this.m, this.h, this.shift, lf(a, this.shift, this.root, b, c), this.J, l)
  }
  if(b === this.h) {
    return a.A(a, c)
  }
  e(Error([G("Index "), G(b), G(" out of bounds  [0,"), G(this.h), G("]")].join("")))
};
var qf = l, qf = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return this.F(this, b);
    case 3:
      return this.t(this, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
w = mf.prototype;
w.call = qf;
w.apply = function(a, b) {
  a = this;
  return a.call.apply(a, [a].concat(b.slice()))
};
w.A = function(a, b) {
  if(32 > this.h - ef(a)) {
    var c = this.J.slice();
    c.push(b);
    return new mf(this.m, this.h + 1, this.shift, this.root, c, l)
  }
  var d = this.h >>> 5 > 1 << this.shift, c = d ? this.shift + 5 : this.shift;
  if(d) {
    d = new df(l, Array(32));
    d.b[0] = this.root;
    var f = ff(l, this.shift, new df(l, this.J));
    d.b[1] = f
  }else {
    d = hf(a, this.shift, this.root, new df(l, this.J))
  }
  return new mf(this.m, this.h + 1, c, d, [b], l)
};
w.Wa = function(a) {
  return a.M(a, 0)
};
w.Xa = function(a) {
  return a.M(a, 1)
};
w.toString = function() {
  return ub(this)
};
w.na = function(a, b) {
  return Eb.a(a, b)
};
w.oa = function(a, b, c) {
  return Eb.c(a, b, c)
};
w.z = function(a) {
  return 0 === this.h ? l : 32 > this.h ? R.e(this.J) : X.c ? X.c(a, 0, 0) : X.call(l, a, 0, 0)
};
w.B = q("h");
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return new mf(b, this.h, this.shift, this.root, this.J, this.l)
};
w.K = q("m");
w.M = function(a, b) {
  return jf(a, b)[b & 31]
};
w.V = function(a, b, c) {
  var d = 0 <= b;
  return(d ? b < this.h : d) ? a.M(a, b) : c
};
var rf = new df(l, Array(32)), sf = new mf(l, 0, 5, rf, [], 0);
function tf(a) {
  var b = a.length;
  if(32 > b) {
    return new mf(l, b, 5, rf, a, l)
  }
  for(var c = a.slice(0, 32), d = 32, f = mb(new mf(l, 32, 5, rf, c, l));;) {
    if(d < b) {
      c = d + 1, f = nb(f, a[d]), d = c
    }else {
      return ob(f)
    }
  }
}
function uf(a) {
  return ob(jd.c(nb, mb(sf), a))
}
function vf(a) {
  var b = l;
  0 < arguments.length && (b = R(Array.prototype.slice.call(arguments, 0), 0));
  return uf(b)
}
vf.n = 0;
vf.k = function(a) {
  a = H(a);
  return uf(a)
};
vf.g = function(a) {
  return uf(a)
};
function Lc(a, b, c, d, f, h) {
  this.S = a;
  this.R = b;
  this.o = c;
  this.w = d;
  this.m = f;
  this.l = h;
  this.j = 31719660;
  this.r = 1536
}
w = Lc.prototype;
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Qb(a)
};
w.ea = function(a) {
  return this.w + 1 < this.R.length ? (a = X.p ? X.p(this.S, this.R, this.o, this.w + 1) : X.call(l, this.S, this.R, this.o, this.w + 1), a == l ? l : a) : a.Ua(a)
};
w.A = function(a, b) {
  return S(b, a)
};
w.toString = function() {
  return ub(this)
};
w.z = aa();
w.O = function() {
  return this.R[this.w]
};
w.P = function(a) {
  return this.w + 1 < this.R.length ? (a = X.p ? X.p(this.S, this.R, this.o, this.w + 1) : X.call(l, this.S, this.R, this.o, this.w + 1), a == l ? wb : a) : a.za(a)
};
w.Ua = function() {
  var a = this.R.length, a = this.o + a < Ca(this.S) ? X.c ? X.c(this.S, this.o + a, 0) : X.call(l, this.S, this.o + a, 0) : l;
  return a == l ? l : a
};
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return X.U ? X.U(this.S, this.R, this.o, this.w, b) : X.call(l, this.S, this.R, this.o, this.w, b)
};
w.Ca = function() {
  return Vd.a(this.R, this.w)
};
w.za = function() {
  var a = this.R.length, a = this.o + a < Ca(this.S) ? X.c ? X.c(this.S, this.o + a, 0) : X.call(l, this.S, this.o + a, 0) : l;
  return a == l ? wb : a
};
var X, wf = l;
function xf(a, b, c) {
  return new Lc(a, jf(a, b), b, c, l, l)
}
function yf(a, b, c, d) {
  return new Lc(a, b, c, d, l, l)
}
function zf(a, b, c, d, f) {
  return new Lc(a, b, c, d, f, l)
}
wf = function(a, b, c, d, f) {
  switch(arguments.length) {
    case 3:
      return xf.call(this, a, b, c);
    case 4:
      return yf.call(this, a, b, c, d);
    case 5:
      return zf.call(this, a, b, c, d, f)
  }
  e(Error("Invalid arity: " + arguments.length))
};
wf.c = xf;
wf.p = yf;
wf.U = zf;
X = wf;
function of(a) {
  return new df({}, a.b.slice())
}
function pf(a) {
  var b = Array(32);
  Mc(a, 0, b, 0, a.length);
  return b
}
var Bf = function Af(b, c, d, f) {
  var d = b.root.q === d.q ? d : new df(b.root.q, d.b.slice()), h = b.h - 1 >>> c & 31;
  if(5 === c) {
    b = f
  }else {
    var i = d.b[h], b = i != l ? Af(b, c - 5, i, f) : ff(b.root.q, c - 5, f)
  }
  d.b[h] = b;
  return d
};
function nf(a, b, c, d) {
  this.h = a;
  this.shift = b;
  this.root = c;
  this.J = d;
  this.j = 275;
  this.r = 88
}
var Cf = l, Cf = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return this.F(this, b);
    case 3:
      return this.t(this, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
w = nf.prototype;
w.call = Cf;
w.apply = function(a, b) {
  a = this;
  return a.call.apply(a, [a].concat(b.slice()))
};
w.F = function(a, b) {
  return a.V(a, b, l)
};
w.t = function(a, b, c) {
  return a.V(a, b, c)
};
w.M = function(a, b) {
  if(this.root.q) {
    return jf(a, b)[b & 31]
  }
  e(Error("nth after persistent!"))
};
w.V = function(a, b, c) {
  var d = 0 <= b;
  return(d ? b < this.h : d) ? a.M(a, b) : c
};
w.B = function() {
  if(this.root.q) {
    return this.h
  }
  e(Error("count after persistent!"))
};
w.pa = function(a, b, c) {
  var d;
  a: {
    if(a.root.q) {
      var f = 0 <= b;
      if(f ? b < a.h : f) {
        ef(a) <= b ? a.J[b & 31] = c : (d = function i(d, f) {
          var n = a.root.q === f.q ? f : new df(a.root.q, f.b.slice());
          if(0 === d) {
            n.b[b & 31] = c
          }else {
            var s = b >>> d & 31, r = i(d - 5, n.b[s]);
            n.b[s] = r
          }
          return n
        }.call(l, a.shift, a.root), a.root = d);
        d = a;
        break a
      }
      if(b === a.h) {
        d = a.qa(a, c);
        break a
      }
      e(Error([G("Index "), G(b), G(" out of bounds for TransientVector of length"), G(a.h)].join("")))
    }
    e(Error("assoc! after persistent!"))
  }
  return d
};
w.qa = function(a, b) {
  if(this.root.q) {
    if(32 > this.h - ef(a)) {
      this.J[this.h & 31] = b
    }else {
      var c = new df(this.root.q, this.J), d = Array(32);
      d[0] = b;
      this.J = d;
      if(this.h >>> 5 > 1 << this.shift) {
        var d = Array(32), f = this.shift + 5;
        d[0] = this.root;
        d[1] = ff(this.root.q, this.shift, c);
        this.root = new df(this.root.q, d);
        this.shift = f
      }else {
        this.root = Bf(a, this.shift, this.root, c)
      }
    }
    this.h += 1;
    return a
  }
  e(Error("conj! after persistent!"))
};
w.va = function(a) {
  if(this.root.q) {
    this.root.q = l;
    var a = this.h - ef(a), b = Array(a);
    Mc(this.J, 0, b, 0, a);
    return new mf(l, this.h, this.shift, this.root, b, l)
  }
  e(Error("persistent! called twice"))
};
function Df() {
  this.r = 0;
  this.j = 2097152
}
Df.prototype.C = t(p);
var Ef = new Df;
function Ff(a, b) {
  var c;
  c = b == l ? 0 : b ? ((c = b.j & 1024) ? c : b.tb) || (b.j ? 0 : z(Pa, b)) : z(Pa, b);
  c = c ? T(a) === T(b) ? Be(Ce, Ke.a(function(a) {
    return O.a(nc.c(b, J(a), Ef), J(L(a)))
  }, a)) : l : l;
  return y(c) ? k : p
}
function Gf(a, b) {
  for(var c = b.length, d = 0;;) {
    if(d < c) {
      if(a === b[d]) {
        return d
      }
      d += 1
    }else {
      return l
    }
  }
}
function Hf(a, b) {
  var c = V.e(a), d = V.e(b);
  return c < d ? -1 : c > d ? 1 : 0
}
function If(a, b, c) {
  for(var d = a.keys, f = d.length, h = a.ja, a = Bc(a), i = 0, j = mb(Jf);;) {
    if(i < f) {
      var m = d[i], i = i + 1, j = pb(j, m, h[m])
    }else {
      return Ac(qe(pb(j, b, c)), a)
    }
  }
}
function Kf(a, b) {
  for(var c = {}, d = b.length, f = 0;;) {
    if(f < d) {
      var h = b[f];
      c[h] = a[h];
      f += 1
    }else {
      break
    }
  }
  return c
}
function Lf(a, b, c, d, f) {
  this.m = a;
  this.keys = b;
  this.ja = c;
  this.Aa = d;
  this.l = f;
  this.r = 4;
  this.j = 16123663
}
w = Lf.prototype;
w.la = function(a) {
  a = Ye(uc.D ? uc.D() : uc.call(l), a);
  return mb(a)
};
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Hd(a)
};
w.F = function(a, b) {
  return a.t(a, b, l)
};
w.t = function(a, b, c) {
  return((a = ba(b)) ? Gf(b, this.keys) != l : a) ? this.ja[b] : c
};
w.da = function(a, b, c) {
  if(ba(b)) {
    var d = this.Aa > Mf;
    if(d ? d : this.keys.length >= Mf) {
      return If(a, b, c)
    }
    if(Gf(b, this.keys) != l) {
      return a = Kf(this.ja, this.keys), a[b] = c, new Lf(this.m, this.keys, a, this.Aa + 1, l)
    }
    a = Kf(this.ja, this.keys);
    d = this.keys.slice();
    a[b] = c;
    d.push(b);
    return new Lf(this.m, d, a, this.Aa + 1, l)
  }
  return If(a, b, c)
};
w.ya = function(a, b) {
  var c = ba(b);
  return(c ? Gf(b, this.keys) != l : c) ? k : p
};
var Nf = l, Nf = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return this.F(this, b);
    case 3:
      return this.t(this, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
w = Lf.prototype;
w.call = Nf;
w.apply = function(a, b) {
  a = this;
  return a.call.apply(a, [a].concat(b.slice()))
};
w.A = function(a, b) {
  return Ic(b) ? a.da(a, B.a(b, 0), B.a(b, 1)) : jd.c(Da, a, b)
};
w.toString = function() {
  return ub(this)
};
w.z = function() {
  var a = this;
  return 0 < a.keys.length ? Ke.a(function(b) {
    return vf.g(R([b, a.ja[b]], 0))
  }, a.keys.sort(Hf)) : l
};
w.B = function() {
  return this.keys.length
};
w.C = function(a, b) {
  return Ff(a, b)
};
w.I = function(a, b) {
  return new Lf(b, this.keys, this.ja, this.Aa, this.l)
};
w.K = q("m");
var Of = new Lf(l, [], {}, 0, 0), Mf = 8;
function Pf(a, b) {
  var c = a.b, d = ba(b);
  if(d ? d : "number" === typeof b) {
    a: {
      for(var d = c.length, f = 0;;) {
        if(d <= f) {
          c = -1;
          break a
        }
        if(b === c[f]) {
          c = f;
          break a
        }
        f += 2
      }
      c = g
    }
  }else {
    if(p) {
      a: {
        for(var d = c.length, f = b.ob, h = 0;;) {
          if(d <= h) {
            c = -1;
            break a
          }
          var i = c[h], j = p;
          if(j ? f === i.ob : j) {
            c = h;
            break a
          }
          h += 2
        }
        c = g
      }
    }else {
      if(b == l) {
        a: {
          d = c.length;
          for(f = 0;;) {
            if(d <= f) {
              c = -1;
              break a
            }
            if(c[f] == l) {
              c = f;
              break a
            }
            f += 2
          }
          c = g
        }
      }else {
        a: {
          d = c.length;
          for(f = 0;;) {
            if(d <= f) {
              c = -1;
              break a
            }
            if(O.a(b, c[f])) {
              c = f;
              break a
            }
            f += 2
          }
          c = g
        }
      }
    }
  }
  return c
}
function Qf(a, b, c, d) {
  this.m = a;
  this.h = b;
  this.b = c;
  this.l = d;
  this.r = 4;
  this.j = 16123663
}
w = Qf.prototype;
w.la = function() {
  return new Rf({}, this.b.length, this.b.slice())
};
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Hd(a)
};
w.F = function(a, b) {
  return a.t(a, b, l)
};
w.t = function(a, b, c) {
  a = Pf(a, b);
  return-1 === a ? c : this.b[a + 1]
};
w.da = function(a, b, c) {
  var d = Pf(a, b);
  if(-1 === d) {
    if(this.h < Tc) {
      for(var d = a.b, a = d.length, f = Array(a + 2), h = 0;;) {
        if(h < a) {
          f[h] = d[h], h += 1
        }else {
          break
        }
      }
      f[a] = b;
      f[a + 1] = c;
      return new Qf(this.m, this.h + 1, f, l)
    }
    return Za(Oa(Ye(Jf, a), b, c), this.m)
  }
  if(c === this.b[d + 1]) {
    return a
  }
  b = this.b.slice();
  b[d + 1] = c;
  return new Qf(this.m, this.h, b, l)
};
w.ya = function(a, b) {
  return-1 !== Pf(a, b)
};
var Sf = l, Sf = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return this.F(this, b);
    case 3:
      return this.t(this, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
w = Qf.prototype;
w.call = Sf;
w.apply = function(a, b) {
  a = this;
  return a.call.apply(a, [a].concat(b.slice()))
};
w.A = function(a, b) {
  return Ic(b) ? a.da(a, B.a(b, 0), B.a(b, 1)) : jd.c(Da, a, b)
};
w.toString = function() {
  return ub(this)
};
w.z = function() {
  var a = this, b;
  if(0 < a.h) {
    var c = a.b.length;
    b = function f(b) {
      return new W(l, p, function() {
        return b < c ? S(tf([a.b[b], a.b[b + 1]]), f(b + 2)) : l
      }, l)
    }(0)
  }else {
    b = l
  }
  return b
};
w.B = q("h");
w.C = function(a, b) {
  return Ff(a, b)
};
w.I = function(a, b) {
  return new Qf(b, this.h, this.b, this.l)
};
w.K = q("m");
var Tf = new Qf(l, 0, [], l), Tc = 8;
function ta(a, b) {
  var c = b ? a : a.slice();
  return new Qf(l, c.length / 2, c, l)
}
function Rf(a, b, c) {
  this.ra = a;
  this.ia = b;
  this.b = c;
  this.r = 56;
  this.j = 258
}
w = Rf.prototype;
w.pa = function(a, b, c) {
  if(y(this.ra)) {
    var d = Pf(a, b);
    if(-1 === d) {
      if(this.ia + 2 <= 2 * Tc) {
        return this.ia += 2, this.b.push(b), this.b.push(c), a
      }
      a = Uf.a ? Uf.a(this.ia, this.b) : Uf.call(l, this.ia, this.b);
      return pb(a, b, c)
    }
    c !== this.b[d + 1] && (this.b[d + 1] = c);
    return a
  }
  e(Error("assoc! after persistent!"))
};
w.qa = function(a, b) {
  if(y(this.ra)) {
    var c;
    c = b ? ((c = b.j & 2048) ? c : b.fb) || (b.j ? 0 : z(Qa, b)) : z(Qa, b);
    if(c) {
      return a.pa(a, Id.e ? Id.e(b) : Id.call(l, b), Jd.e ? Jd.e(b) : Jd.call(l, b))
    }
    c = H(b);
    for(var d = a;;) {
      var f = J(c);
      if(y(f)) {
        c = L(c), d = d.pa(d, Id.e ? Id.e(f) : Id.call(l, f), Jd.e ? Jd.e(f) : Jd.call(l, f))
      }else {
        return d
      }
    }
  }else {
    e(Error("conj! after persistent!"))
  }
};
w.va = function() {
  if(y(this.ra)) {
    return this.ra = p, new Qf(l, td(this.ia), this.b, l)
  }
  e(Error("persistent! called twice"))
};
w.F = function(a, b) {
  return a.t(a, b, l)
};
w.t = function(a, b, c) {
  if(y(this.ra)) {
    return a = Pf(a, b), -1 === a ? c : this.b[a + 1]
  }
  e(Error("lookup after persistent!"))
};
w.B = function() {
  if(y(this.ra)) {
    return td(this.ia)
  }
  e(Error("count after persistent!"))
};
function Uf(a, b) {
  for(var c = mb(Of), d = 0;;) {
    if(d < a) {
      c = pb(c, b[d], b[d + 1]), d += 2
    }else {
      return c
    }
  }
}
function Vf() {
  this.$ = p
}
function Wf(a, b) {
  return ba(a) ? a === b : O.a(a, b)
}
var Xf, Yf = l;
function Zf(a, b, c) {
  a = a.slice();
  a[b] = c;
  return a
}
function $f(a, b, c, d, f) {
  a = a.slice();
  a[b] = c;
  a[d] = f;
  return a
}
Yf = function(a, b, c, d, f) {
  switch(arguments.length) {
    case 3:
      return Zf.call(this, a, b, c);
    case 5:
      return $f.call(this, a, b, c, d, f)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Yf.c = Zf;
Yf.U = $f;
Xf = Yf;
var ag, bg = l;
function cg(a, b, c, d) {
  a = a.sa(b);
  a.b[c] = d;
  return a
}
function dg(a, b, c, d, f, h) {
  a = a.sa(b);
  a.b[c] = d;
  a.b[f] = h;
  return a
}
bg = function(a, b, c, d, f, h) {
  switch(arguments.length) {
    case 4:
      return cg.call(this, a, b, c, d);
    case 6:
      return dg.call(this, a, b, c, d, f, h)
  }
  e(Error("Invalid arity: " + arguments.length))
};
bg.p = cg;
bg.aa = dg;
ag = bg;
function eg(a, b, c) {
  this.q = a;
  this.v = b;
  this.b = c
}
w = eg.prototype;
w.X = function(a, b, c, d, f, h) {
  var i = 1 << (c >>> b & 31), j = ud(this.v & i - 1);
  if(0 === (this.v & i)) {
    var m = ud(this.v);
    if(2 * m < this.b.length) {
      a = this.sa(a);
      b = a.b;
      h.$ = k;
      a: {
        c = 2 * (m - j);
        h = 2 * j + (c - 1);
        for(m = 2 * (j + 1) + (c - 1);;) {
          if(0 === c) {
            break a
          }
          b[m] = b[h];
          m -= 1;
          c -= 1;
          h -= 1
        }
      }
      b[2 * j] = d;
      b[2 * j + 1] = f;
      a.v |= i;
      return a
    }
    if(16 <= m) {
      j = Array(32);
      j[c >>> b & 31] = fg.X(a, b + 5, c, d, f, h);
      for(f = d = 0;;) {
        if(32 > d) {
          0 !== (this.v >>> d & 1) && (j[d] = this.b[f] != l ? fg.X(a, b + 5, V.e(this.b[f]), this.b[f], this.b[f + 1], h) : this.b[f + 1], f += 2), d += 1
        }else {
          break
        }
      }
      return new gg(a, m + 1, j)
    }
    b = Array(2 * (m + 4));
    Mc(this.b, 0, b, 0, 2 * j);
    b[2 * j] = d;
    b[2 * j + 1] = f;
    Mc(this.b, 2 * j, b, 2 * (j + 1), 2 * (m - j));
    h.$ = k;
    a = this.sa(a);
    a.b = b;
    a.v |= i;
    return a
  }
  m = this.b[2 * j];
  i = this.b[2 * j + 1];
  if(m == l) {
    return m = i.X(a, b + 5, c, d, f, h), m === i ? this : ag.p(this, a, 2 * j + 1, m)
  }
  if(Wf(d, m)) {
    return f === i ? this : ag.p(this, a, 2 * j + 1, f)
  }
  h.$ = k;
  return ag.aa(this, a, 2 * j, l, 2 * j + 1, hg.ma ? hg.ma(a, b + 5, m, i, c, d, f) : hg.call(l, a, b + 5, m, i, c, d, f))
};
w.xa = function() {
  return ig.e ? ig.e(this.b) : ig.call(l, this.b)
};
w.sa = function(a) {
  if(a === this.q) {
    return this
  }
  var b = ud(this.v), c = Array(0 > b ? 4 : 2 * (b + 1));
  Mc(this.b, 0, c, 0, 2 * b);
  return new eg(a, this.v, c)
};
w.W = function(a, b, c, d, f) {
  var h = 1 << (b >>> a & 31), i = ud(this.v & h - 1);
  if(0 === (this.v & h)) {
    var j = ud(this.v);
    if(16 <= j) {
      i = Array(32);
      i[b >>> a & 31] = fg.W(a + 5, b, c, d, f);
      for(d = c = 0;;) {
        if(32 > c) {
          0 !== (this.v >>> c & 1) && (i[c] = this.b[d] != l ? fg.W(a + 5, V.e(this.b[d]), this.b[d], this.b[d + 1], f) : this.b[d + 1], d += 2), c += 1
        }else {
          break
        }
      }
      return new gg(l, j + 1, i)
    }
    a = Array(2 * (j + 1));
    Mc(this.b, 0, a, 0, 2 * i);
    a[2 * i] = c;
    a[2 * i + 1] = d;
    Mc(this.b, 2 * i, a, 2 * (i + 1), 2 * (j - i));
    f.$ = k;
    return new eg(l, this.v | h, a)
  }
  j = this.b[2 * i];
  h = this.b[2 * i + 1];
  if(j == l) {
    return j = h.W(a + 5, b, c, d, f), j === h ? this : new eg(l, this.v, Xf.c(this.b, 2 * i + 1, j))
  }
  if(Wf(c, j)) {
    return d === h ? this : new eg(l, this.v, Xf.c(this.b, 2 * i + 1, d))
  }
  f.$ = k;
  return new eg(l, this.v, Xf.U(this.b, 2 * i, l, 2 * i + 1, hg.aa ? hg.aa(a + 5, j, h, b, c, d) : hg.call(l, a + 5, j, h, b, c, d)))
};
w.ga = function(a, b, c, d) {
  var f = 1 << (b >>> a & 31);
  if(0 === (this.v & f)) {
    return d
  }
  var h = ud(this.v & f - 1), f = this.b[2 * h], h = this.b[2 * h + 1];
  return f == l ? h.ga(a + 5, b, c, d) : Wf(c, f) ? h : d
};
var fg = new eg(l, 0, []);
function gg(a, b, c) {
  this.q = a;
  this.h = b;
  this.b = c
}
w = gg.prototype;
w.X = function(a, b, c, d, f, h) {
  var i = c >>> b & 31, j = this.b[i];
  if(j == l) {
    return a = ag.p(this, a, i, fg.X(a, b + 5, c, d, f, h)), a.h += 1, a
  }
  b = j.X(a, b + 5, c, d, f, h);
  return b === j ? this : ag.p(this, a, i, b)
};
w.xa = function() {
  return jg.e ? jg.e(this.b) : jg.call(l, this.b)
};
w.sa = function(a) {
  return a === this.q ? this : new gg(a, this.h, this.b.slice())
};
w.W = function(a, b, c, d, f) {
  var h = b >>> a & 31, i = this.b[h];
  if(i == l) {
    return new gg(l, this.h + 1, Xf.c(this.b, h, fg.W(a + 5, b, c, d, f)))
  }
  a = i.W(a + 5, b, c, d, f);
  return a === i ? this : new gg(l, this.h, Xf.c(this.b, h, a))
};
w.ga = function(a, b, c, d) {
  var f = this.b[b >>> a & 31];
  return f != l ? f.ga(a + 5, b, c, d) : d
};
function kg(a, b, c) {
  for(var b = 2 * b, d = 0;;) {
    if(d < b) {
      if(Wf(c, a[d])) {
        return d
      }
      d += 2
    }else {
      return-1
    }
  }
}
function lg(a, b, c, d) {
  this.q = a;
  this.fa = b;
  this.h = c;
  this.b = d
}
w = lg.prototype;
w.X = function(a, b, c, d, f, h) {
  if(c === this.fa) {
    b = kg(this.b, this.h, d);
    if(-1 === b) {
      if(this.b.length > 2 * this.h) {
        return a = ag.aa(this, a, 2 * this.h, d, 2 * this.h + 1, f), h.$ = k, a.h += 1, a
      }
      c = this.b.length;
      b = Array(c + 2);
      Mc(this.b, 0, b, 0, c);
      b[c] = d;
      b[c + 1] = f;
      h.$ = k;
      h = this.h + 1;
      a === this.q ? (this.b = b, this.h = h, a = this) : a = new lg(this.q, this.fa, h, b);
      return a
    }
    return this.b[b + 1] === f ? this : ag.p(this, a, b + 1, f)
  }
  return(new eg(a, 1 << (this.fa >>> b & 31), [l, this, l, l])).X(a, b, c, d, f, h)
};
w.xa = function() {
  return ig.e ? ig.e(this.b) : ig.call(l, this.b)
};
w.sa = function(a) {
  if(a === this.q) {
    return this
  }
  var b = Array(2 * (this.h + 1));
  Mc(this.b, 0, b, 0, 2 * this.h);
  return new lg(a, this.fa, this.h, b)
};
w.W = function(a, b, c, d, f) {
  return b === this.fa ? (a = kg(this.b, this.h, c), -1 === a ? (a = this.b.length, b = Array(a + 2), Mc(this.b, 0, b, 0, a), b[a] = c, b[a + 1] = d, f.$ = k, new lg(l, this.fa, this.h + 1, b)) : O.a(this.b[a], d) ? this : new lg(l, this.fa, this.h, Xf.c(this.b, a + 1, d))) : (new eg(l, 1 << (this.fa >>> a & 31), [l, this])).W(a, b, c, d, f)
};
w.ga = function(a, b, c, d) {
  a = kg(this.b, this.h, c);
  return 0 > a ? d : Wf(c, this.b[a]) ? this.b[a + 1] : d
};
var hg, mg = l;
function ng(a, b, c, d, f, h) {
  var i = V.e(b);
  if(i === d) {
    return new lg(l, i, 2, [b, c, f, h])
  }
  var j = new Vf;
  return fg.W(a, i, b, c, j).W(a, d, f, h, j)
}
function og(a, b, c, d, f, h, i) {
  var j = V.e(c);
  if(j === f) {
    return new lg(l, j, 2, [c, d, h, i])
  }
  var m = new Vf;
  return fg.X(a, b, j, c, d, m).X(a, b, f, h, i, m)
}
mg = function(a, b, c, d, f, h, i) {
  switch(arguments.length) {
    case 6:
      return ng.call(this, a, b, c, d, f, h);
    case 7:
      return og.call(this, a, b, c, d, f, h, i)
  }
  e(Error("Invalid arity: " + arguments.length))
};
mg.aa = ng;
mg.ma = og;
hg = mg;
function pg(a, b, c, d, f) {
  this.m = a;
  this.Y = b;
  this.o = c;
  this.Z = d;
  this.l = f;
  this.r = 0;
  this.j = 31850572
}
w = pg.prototype;
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Qb(a)
};
w.A = function(a, b) {
  return S(b, a)
};
w.toString = function() {
  return ub(this)
};
w.z = aa();
w.O = function() {
  return this.Z == l ? tf([this.Y[this.o], this.Y[this.o + 1]]) : J(this.Z)
};
w.P = function() {
  return this.Z == l ? ig.c ? ig.c(this.Y, this.o + 2, l) : ig.call(l, this.Y, this.o + 2, l) : ig.c ? ig.c(this.Y, this.o, L(this.Z)) : ig.call(l, this.Y, this.o, L(this.Z))
};
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return new pg(b, this.Y, this.o, this.Z, this.l)
};
w.K = q("m");
var ig, qg = l;
function rg(a) {
  return qg.c(a, 0, l)
}
function sg(a, b, c) {
  if(c == l) {
    for(c = a.length;;) {
      if(b < c) {
        if(a[b] != l) {
          return new pg(l, a, b, l, l)
        }
        var d = a[b + 1];
        if(y(d) && (d = d.xa(), y(d))) {
          return new pg(l, a, b + 2, d, l)
        }
        b += 2
      }else {
        return l
      }
    }
  }else {
    return new pg(l, a, b, c, l)
  }
}
qg = function(a, b, c) {
  switch(arguments.length) {
    case 1:
      return rg.call(this, a);
    case 3:
      return sg.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
qg.e = rg;
qg.c = sg;
ig = qg;
function tg(a, b, c, d, f) {
  this.m = a;
  this.Y = b;
  this.o = c;
  this.Z = d;
  this.l = f;
  this.r = 0;
  this.j = 31850572
}
w = tg.prototype;
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Qb(a)
};
w.A = function(a, b) {
  return S(b, a)
};
w.toString = function() {
  return ub(this)
};
w.z = aa();
w.O = function() {
  return J(this.Z)
};
w.P = function() {
  return jg.p ? jg.p(l, this.Y, this.o, L(this.Z)) : jg.call(l, l, this.Y, this.o, L(this.Z))
};
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return new tg(b, this.Y, this.o, this.Z, this.l)
};
w.K = q("m");
var jg, ug = l;
function vg(a) {
  return ug.p(l, a, 0, l)
}
function wg(a, b, c, d) {
  if(d == l) {
    for(d = b.length;;) {
      if(c < d) {
        var f = b[c];
        if(y(f) && (f = f.xa(), y(f))) {
          return new tg(a, b, c + 1, f, l)
        }
        c += 1
      }else {
        return l
      }
    }
  }else {
    return new tg(a, b, c, d, l)
  }
}
ug = function(a, b, c, d) {
  switch(arguments.length) {
    case 1:
      return vg.call(this, a);
    case 4:
      return wg.call(this, a, b, c, d)
  }
  e(Error("Invalid arity: " + arguments.length))
};
ug.e = vg;
ug.p = wg;
jg = ug;
function xg(a, b, c, d, f, h) {
  this.m = a;
  this.h = b;
  this.root = c;
  this.N = d;
  this.Q = f;
  this.l = h;
  this.r = 4;
  this.j = 16123663
}
w = xg.prototype;
w.la = function() {
  return new yg({}, this.root, this.h, this.N, this.Q)
};
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Hd(a)
};
w.F = function(a, b) {
  return a.t(a, b, l)
};
w.t = function(a, b, c) {
  return b == l ? this.N ? this.Q : c : this.root == l ? c : this.root.ga(0, V.e(b), b, c)
};
w.da = function(a, b, c) {
  if(b == l) {
    var d = this.N;
    return(d ? c === this.Q : d) ? a : new xg(this.m, this.N ? this.h : this.h + 1, this.root, k, c, l)
  }
  d = new Vf;
  c = (this.root == l ? fg : this.root).W(0, V.e(b), b, c, d);
  return c === this.root ? a : new xg(this.m, d.$ ? this.h + 1 : this.h, c, this.N, this.Q, l)
};
w.ya = function(a, b) {
  return b == l ? this.N : this.root == l ? p : this.root.ga(0, V.e(b), b, Nc) !== Nc
};
var zg = l, zg = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return this.F(this, b);
    case 3:
      return this.t(this, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
w = xg.prototype;
w.call = zg;
w.apply = function(a, b) {
  a = this;
  return a.call.apply(a, [a].concat(b.slice()))
};
w.A = function(a, b) {
  return Ic(b) ? a.da(a, B.a(b, 0), B.a(b, 1)) : jd.c(Da, a, b)
};
w.toString = function() {
  return ub(this)
};
w.z = function() {
  if(0 < this.h) {
    var a = this.root != l ? this.root.xa() : l;
    return this.N ? S(tf([l, this.Q]), a) : a
  }
  return l
};
w.B = q("h");
w.C = function(a, b) {
  return Ff(a, b)
};
w.I = function(a, b) {
  return new xg(b, this.h, this.root, this.N, this.Q, this.l)
};
w.K = q("m");
var Jf = new xg(l, 0, l, p, l, 0);
function yg(a, b, c, d, f) {
  this.q = a;
  this.root = b;
  this.count = c;
  this.N = d;
  this.Q = f;
  this.r = 56;
  this.j = 258
}
w = yg.prototype;
w.pa = function(a, b, c) {
  return Ag(a, b, c)
};
w.qa = function(a, b) {
  var c;
  a: {
    if(a.q) {
      c = b ? ((c = b.j & 2048) ? c : b.fb) || (b.j ? 0 : z(Qa, b)) : z(Qa, b);
      if(c) {
        c = Ag(a, Id.e ? Id.e(b) : Id.call(l, b), Jd.e ? Jd.e(b) : Jd.call(l, b));
        break a
      }
      c = H(b);
      for(var d = a;;) {
        var f = J(c);
        if(y(f)) {
          c = L(c), d = Ag(d, Id.e ? Id.e(f) : Id.call(l, f), Jd.e ? Jd.e(f) : Jd.call(l, f))
        }else {
          c = d;
          break a
        }
      }
    }else {
      e(Error("conj! after persistent"))
    }
    c = g
  }
  return c
};
w.va = function(a) {
  var b;
  a.q ? (a.q = l, b = new xg(l, a.count, a.root, a.N, a.Q, l)) : e(Error("persistent! called twice"));
  return b
};
w.F = function(a, b) {
  return b == l ? this.N ? this.Q : l : this.root == l ? l : this.root.ga(0, V.e(b), b)
};
w.t = function(a, b, c) {
  return b == l ? this.N ? this.Q : c : this.root == l ? c : this.root.ga(0, V.e(b), b, c)
};
w.B = function() {
  if(this.q) {
    return this.count
  }
  e(Error("count after persistent!"))
};
function Ag(a, b, c) {
  if(a.q) {
    if(b == l) {
      a.Q !== c && (a.Q = c), a.N || (a.count += 1, a.N = k)
    }else {
      var d = new Vf, b = (a.root == l ? fg : a.root).X(a.q, 0, V.e(b), b, c, d);
      b !== a.root && (a.root = b);
      d.$ && (a.count += 1)
    }
    return a
  }
  e(Error("assoc! after persistent!"))
}
var uc;
function Bg(a) {
  for(var b = H(a), c = mb(Jf);;) {
    if(b) {
      var a = L(L(b)), d = J(b), b = J(L(b)), c = pb(c, d, b), b = a
    }else {
      return ob(c)
    }
  }
}
function Cg(a) {
  var b = l;
  0 < arguments.length && (b = R(Array.prototype.slice.call(arguments, 0), 0));
  return Bg.call(this, b)
}
Cg.n = 0;
Cg.k = function(a) {
  a = H(a);
  return Bg(a)
};
Cg.g = Bg;
uc = Cg;
function Dg(a) {
  return new Qf(l, td(T(a)), zc.a(ya, a), l)
}
function Eg(a) {
  var b = l;
  0 < arguments.length && (b = R(Array.prototype.slice.call(arguments, 0), 0));
  return Dg.call(this, b)
}
Eg.n = 0;
Eg.k = function(a) {
  a = H(a);
  return Dg(a)
};
Eg.g = Dg;
function Id(a) {
  return Ra(a)
}
function Jd(a) {
  return Sa(a)
}
function Uc(a, b, c) {
  this.m = a;
  this.ta = b;
  this.l = c;
  this.r = 4;
  this.j = 15077647
}
Uc.prototype.la = function() {
  return new Fg(mb(this.ta))
};
Uc.prototype.G = function(a) {
  var b = this.l;
  if(b != l) {
    return b
  }
  a: {
    b = 0;
    for(a = H(a);;) {
      if(a) {
        var c = J(a), b = (b + V.e(c)) % 4503599627370496, a = L(a)
      }else {
        break a
      }
    }
    b = g
  }
  return this.l = b
};
Uc.prototype.F = function(a, b) {
  return a.t(a, b, l)
};
Uc.prototype.t = function(a, b, c) {
  return y(Na(this.ta, b)) ? b : c
};
var Gg = l, Gg = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return this.F(this, b);
    case 3:
      return this.t(this, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
w = Uc.prototype;
w.call = Gg;
w.apply = function(a, b) {
  a = this;
  return a.call.apply(a, [a].concat(b.slice()))
};
w.A = function(a, b) {
  return new Uc(this.m, rc.c(this.ta, b, l), l)
};
w.toString = function() {
  return ub(this)
};
w.z = function() {
  return H(Ke.a(J, this.ta))
};
w.B = function() {
  return Ca(this.ta)
};
w.C = function(a, b) {
  var c;
  c = b == l ? p : b ? ((c = b.j & 4096) ? c : b.xb) ? k : b.j ? p : z(Ua, b) : z(Ua, b);
  return c ? (c = T(a) === T(b)) ? Be(function(b) {
    return nc.c(a, b, Nc) === Nc ? p : k
  }, b) : c : c
};
w.I = function(a, b) {
  return new Uc(b, this.ta, this.l)
};
w.K = q("m");
var Vc = new Uc(l, Tf, 0);
function Fg(a) {
  this.ka = a;
  this.j = 259;
  this.r = 136
}
var Hg = l, Hg = function(a, b, c) {
  switch(arguments.length) {
    case 2:
      return Ja.c(this.ka, b, Nc) === Nc ? l : b;
    case 3:
      return Ja.c(this.ka, b, Nc) === Nc ? c : b
  }
  e(Error("Invalid arity: " + arguments.length))
};
w = Fg.prototype;
w.call = Hg;
w.apply = function(a, b) {
  a = this;
  return a.call.apply(a, [a].concat(b.slice()))
};
w.F = function(a, b) {
  return a.t(a, b, l)
};
w.t = function(a, b, c) {
  return Ja.c(this.ka, b, Nc) === Nc ? c : b
};
w.B = function() {
  return T(this.ka)
};
w.qa = function(a, b) {
  this.ka = pb(this.ka, b, l);
  return a
};
w.va = function() {
  return new Uc(l, ob(this.ka), l)
};
function Ig(a) {
  if(a && y(y(l) ? l : a.hb)) {
    return a.name
  }
  if(wa(a)) {
    return a
  }
  if(Oc(a)) {
    var b = a.lastIndexOf("/", a.length - 2);
    return 0 > b ? Ed.a(a, 2) : Ed.a(a, b + 1)
  }
  e(Error([G("Doesn't support name: "), G(a)].join("")))
}
function Jg(a) {
  if(a && y(y(l) ? l : a.hb)) {
    return a.Bb
  }
  if(Oc(a)) {
    var b = a.lastIndexOf("/", a.length - 2);
    return-1 < b ? Ed.c(a, 2, b) : l
  }
  e(Error([G("Doesn't support namespace: "), G(a)].join("")))
}
function Og(a, b, c, d, f) {
  this.m = a;
  this.start = b;
  this.end = c;
  this.step = d;
  this.l = f;
  this.r = 0;
  this.j = 32375006
}
w = Og.prototype;
w.G = function(a) {
  var b = this.l;
  return b != l ? b : this.l = a = Qb(a)
};
w.ea = function() {
  return 0 < this.step ? this.start + this.step < this.end ? new Og(this.m, this.start + this.step, this.end, this.step, l) : l : this.start + this.step > this.end ? new Og(this.m, this.start + this.step, this.end, this.step, l) : l
};
w.A = function(a, b) {
  return S(b, a)
};
w.toString = function() {
  return ub(this)
};
w.na = function(a, b) {
  return Eb.a(a, b)
};
w.oa = function(a, b, c) {
  return Eb.c(a, b, c)
};
w.z = function(a) {
  return 0 < this.step ? this.start < this.end ? a : l : this.start > this.end ? a : l
};
w.B = function(a) {
  a = a.z(a);
  return!y(a) ? 0 : Math.ceil((this.end - this.start) / this.step)
};
w.O = q("start");
w.P = function(a) {
  return a.z(a) != l ? new Og(this.m, this.start + this.step, this.end, this.step, l) : wb
};
w.C = function(a, b) {
  return Sb(a, b)
};
w.I = function(a, b) {
  return new Og(b, this.start, this.end, this.step, this.l)
};
w.K = q("m");
w.M = function(a, b) {
  if(b < a.B(a)) {
    return this.start + b * this.step
  }
  var c = this.start > this.end;
  if(c ? 0 === this.step : c) {
    return this.start
  }
  e(Error("Index out of bounds"))
};
w.V = function(a, b, c) {
  c = b < a.B(a) ? this.start + b * this.step : ((a = this.start > this.end) ? 0 === this.step : a) ? this.start : c;
  return c
};
var Pg, Qg = l;
function Rg() {
  return Qg.c(0, Number.MAX_VALUE, 1)
}
function Sg(a) {
  return Qg.c(0, a, 1)
}
function Tg(a, b) {
  return Qg.c(a, b, 1)
}
function Ug(a, b, c) {
  return new Og(l, a, b, c, l)
}
Qg = function(a, b, c) {
  switch(arguments.length) {
    case 0:
      return Rg.call(this);
    case 1:
      return Sg.call(this, a);
    case 2:
      return Tg.call(this, a, b);
    case 3:
      return Ug.call(this, a, b, c)
  }
  e(Error("Invalid arity: " + arguments.length))
};
Qg.D = Rg;
Qg.e = Sg;
Qg.a = Tg;
Qg.c = Ug;
Pg = Qg;
function Y(a, b, c, d, f, h, i) {
  F(a, c);
  H(i) && (b.c ? b.c(J(i), a, h) : b.call(l, J(i), a, h));
  for(var c = H(L(i)), i = l, j = 0, m = 0;;) {
    if(m < j) {
      var n = i.M(i, m);
      F(a, d);
      b.c ? b.c(n, a, h) : b.call(l, n, a, h);
      m += 1
    }else {
      if(c = H(c)) {
        i = c, Jc(i) ? (c = rb(i), m = sb(i), i = c, j = T(c), c = m) : (c = J(i), F(a, d), b.c ? b.c(c, a, h) : b.call(l, c, a, h), c = L(i), i = l, j = 0), m = 0
      }else {
        break
      }
    }
  }
  return F(a, f)
}
function Vg(a, b) {
  for(var c = H(b), d = l, f = 0, h = 0;;) {
    if(h < f) {
      var i = d.M(d, h);
      F(a, i);
      h += 1
    }else {
      if(c = H(c)) {
        d = c, Jc(d) ? (c = rb(d), f = sb(d), d = c, i = T(c), c = f, f = i) : (i = J(d), F(a, i), c = L(d), d = l, f = 0), h = 0
      }else {
        return l
      }
    }
  }
}
function Wg(a, b) {
  var c = l;
  1 < arguments.length && (c = R(Array.prototype.slice.call(arguments, 1), 0));
  return Vg.call(this, a, c)
}
Wg.n = 1;
Wg.k = function(a) {
  var b = J(a), a = K(a);
  return Vg(b, a)
};
Wg.g = Vg;
function Xg(a) {
  ma.e ? ma.e(a) : ma.call(l, a);
  return l
}
var Yg = {'"':'\\"', "\\":"\\\\", "\b":"\\b", "\f":"\\f", "\n":"\\n", "\r":"\\r", "\t":"\\t"}, Z = function Zg(b, c, d) {
  if(b == l) {
    return F(c, "nil")
  }
  if(g === b) {
    return F(c, "#<undefined>")
  }
  var f;
  f = nc.a(d, "\ufdd0:meta");
  y(f) && (f = b ? ((f = b.j & 131072) ? f : b.gb) ? k : b.j ? p : z(Wa, b) : z(Wa, b), f = y(f) ? Bc(b) : f);
  y(f) && (F(c, "^"), Zg(Bc(b), c, d), F(c, " "));
  if(b == l) {
    return F(c, "nil")
  }
  if(b.$a) {
    return b.kb(c)
  }
  if(f = b) {
    f = (f = b.j & 2147483648) ? f : b.L
  }
  return f ? b.H(b, c, d) : ((f = (b == l ? l : b.constructor) === Boolean) ? f : "number" === typeof b) ? F(c, "" + G(b)) : b instanceof Array ? Y(c, Zg, "#<Array [", ", ", "]>", d, b) : ba(b) ? Oc(b) ? (F(c, ":"), d = Jg(b), y(d) && Wg.g(c, R(["" + G(d), "/"], 0)), F(c, Ig(b))) : p ? (d = Jg(b), y(d) && Wg.g(c, R(["" + G(d), "/"], 0)), F(c, Ig(b))) : y((new Pd("\ufdd0:readably")).call(l, d)) ? F(c, [G('"'), G(b.replace(RegExp('[\\\\"\b\f\n\r\t]', "g"), function(b) {
    return Yg[b]
  })), G('"')].join("")) : F(c, b) : xc(b) ? Wg.g(c, R(["#<", "" + G(b), ">"], 0)) : b instanceof Date ? (d = function(b, c) {
    for(var d = "" + G(b);;) {
      if(T(d) < c) {
        d = [G("0"), G(d)].join("")
      }else {
        return d
      }
    }
  }, Wg.g(c, R(['#inst "', "" + G(b.getUTCFullYear()), "-", d(b.getUTCMonth() + 1, 2), "-", d(b.getUTCDate(), 2), "T", d(b.getUTCHours(), 2), ":", d(b.getUTCMinutes(), 2), ":", d(b.getUTCSeconds(), 2), ".", d(b.getUTCMilliseconds(), 3), "-", '00:00"'], 0))) : y(b instanceof RegExp) ? Wg.g(c, R(['#"', b.source, '"'], 0)) : Wg.g(c, R(["#<", "" + G(b), ">"], 0))
};
function $g(a) {
  var b = rc.c(sa(), "\ufdd0:readably", p), c = Xg, d = a == l;
  d || (d = H(a), d = y(d) ? p : k);
  if(d) {
    b = ""
  }else {
    var d = G, f = new ka, h = new tb(f);
    a: {
      Z(J(a), h, b);
      for(var a = H(L(a)), i = l, j = 0, m = 0;;) {
        if(m < j) {
          var n = i.M(i, m);
          F(h, " ");
          Z(n, h, b);
          m += 1
        }else {
          if(a = H(a)) {
            i = a, Jc(i) ? (a = rb(i), j = sb(i), i = a, n = T(a), a = j, j = n) : (n = J(i), F(h, " "), Z(n, h, b), a = L(i), i = l, j = 0), m = 0
          }else {
            break a
          }
        }
      }
    }
    lb(h);
    b = "" + d(f)
  }
  c(b);
  c = sa();
  Xg("\n");
  return nc.a(c, "\ufdd0:flush-on-newline"), l
}
function ah(a) {
  var b = l;
  0 < arguments.length && (b = R(Array.prototype.slice.call(arguments, 0), 0));
  return $g.call(this, b)
}
ah.n = 0;
ah.k = function(a) {
  a = H(a);
  return $g(a)
};
ah.g = $g;
vb.prototype.L = k;
vb.prototype.H = function(a, b, c) {
  return Y(b, Z, "(", " ", ")", c, a)
};
Kc.prototype.L = k;
Kc.prototype.H = function(a, b, c) {
  return Y(b, Z, "(", " ", ")", c, a)
};
Qf.prototype.L = k;
Qf.prototype.H = function(a, b, c) {
  return Y(b, function(a) {
    return Y(b, Z, "", " ", "", c, a)
  }, "{", ", ", "}", c, a)
};
W.prototype.L = k;
W.prototype.H = function(a, b, c) {
  return Y(b, Z, "(", " ", ")", c, a)
};
pg.prototype.L = k;
pg.prototype.H = function(a, b, c) {
  return Y(b, Z, "(", " ", ")", c, a)
};
Lc.prototype.L = k;
Lc.prototype.H = function(a, b, c) {
  return Y(b, Z, "(", " ", ")", c, a)
};
xg.prototype.L = k;
xg.prototype.H = function(a, b, c) {
  return Y(b, function(a) {
    return Y(b, Z, "", " ", "", c, a)
  }, "{", ", ", "}", c, a)
};
Uc.prototype.L = k;
Uc.prototype.H = function(a, b, c) {
  return Y(b, Z, "#{", " ", "}", c, a)
};
mf.prototype.L = k;
mf.prototype.H = function(a, b, c) {
  return Y(b, Z, "[", " ", "]", c, a)
};
Kd.prototype.L = k;
Kd.prototype.H = function(a, b, c) {
  return Y(b, Z, "(", " ", ")", c, a)
};
Ld.prototype.L = k;
Ld.prototype.H = function(a, b) {
  return F(b, "()")
};
Od.prototype.L = k;
Od.prototype.H = function(a, b, c) {
  return Y(b, Z, "(", " ", ")", c, a)
};
Og.prototype.L = k;
Og.prototype.H = function(a, b, c) {
  return Y(b, Z, "(", " ", ")", c, a)
};
tg.prototype.L = k;
tg.prototype.H = function(a, b, c) {
  return Y(b, Z, "(", " ", ")", c, a)
};
Lf.prototype.L = k;
Lf.prototype.H = function(a, b, c) {
  return Y(b, function(a) {
    return Y(b, Z, "", " ", "", c, a)
  }, "{", ", ", "}", c, a)
};
mf.prototype.bb = k;
mf.prototype.cb = function(a, b) {
  return Yc.a(a, b)
};
var ch = function bh(b) {
  return y(b) ? S(b, new W(l, p, function() {
    var c;
    a: {
      c = T(b);
      var d;
      b: {
        for(var f = c - 2;;) {
          if(O.a(f, -1)) {
            d = l;
            break b
          }
          if((b.e ? b.e(f) : b.call(l, f)) < (b.e ? b.e(f + 1) : b.call(l, f + 1))) {
            d = f;
            break b
          }
          f -= 1
        }
        d = g
      }
      if(y(d)) {
        var f = b.e ? b.e(d) : b.call(l, d), h;
        b: {
          for(h = c - 1;;) {
            if(f < (b.e ? b.e(h) : b.call(l, h))) {
              break b
            }
            h -= 1
          }
          h = g
        }
        f = rc.g(b, d, b.e ? b.e(h) : b.call(l, h), R([h, f], 0));
        d += 1;
        for(c -= 1;;) {
          if(d < c) {
            f = rc.g(f, d, f.e ? f.e(c) : f.call(l, c), R([c, f.e ? f.e(d) : f.call(l, d)], 0)), d += 1, c -= 1
          }else {
            c = f;
            break a
          }
        }
        c = g
      }else {
        c = l
      }
    }
    return bh(c)
  }, l)) : l
};
function dh(a) {
  return new W(l, p, function() {
    var b = uf(bd.e(a));
    return 0 === T(b) ? Rb.g(R([sf], 0)) : ch(b)
  }, l)
}
function eh(a) {
  var b;
  b = (b = Be(va, a)) ? Be(De.a(zc, pd), Ze.c(2, 1, a)) : b;
  if(y(b)) {
    return dh(a)
  }
  if(y(zc.a(Pc, a))) {
    var c = uf(a);
    return Ke.a(function(a) {
      return Ke.a(c, a)
    }, dh(Pg.e(T(c))))
  }
  var d = qe(jd.c(function(a, b) {
    var c = nc.c(a, b, 0) + 1;
    return pb(a, b, c)
  }, mb(Of), a)), f = uf(H(Ke.a(J, d))), a = zc.a(de, function i(a) {
    return new W(l, p, function() {
      for(;;) {
        var b = H(a);
        if(b) {
          if(Jc(b)) {
            var c = rb(b), s = T(c), r = new Td(Array(s), 0);
            a: {
              for(var v = 0;;) {
                if(v < s) {
                  var u = B.a(c, v), u = Ue.a(d.e ? d.e(f.e ? f.e(u) : f.call(l, u)) : d.call(l, f.e ? f.e(u) : f.call(l, u)), u);
                  r.add(u);
                  v += 1
                }else {
                  c = k;
                  break a
                }
              }
              c = g
            }
            return c ? $d(r.T(), i(sb(b))) : $d(r.T(), l)
          }
          r = J(b);
          return S(Ue.a(d.e ? d.e(f.e ? f.e(r) : f.call(l, r)) : d.call(l, f.e ? f.e(r) : f.call(l, r)), r), i(K(b)))
        }
        return l
      }
    }, l)
  }(Pg.e(T(f))));
  return Ke.a(De.a(Ke, f), dh(a))
}
;var fh = require, gh = process, Xg = (fh.e ? fh.e("util") : fh.call(l, "util")).print;
function $(a, b) {
  return O.a(U.a(a, 0), b) ? 0 : O.a(U.a(a, 1), b) ? 1 : O.a(U.a(a, 2), b) ? 2 : O.a(U.a(a, 3), b) ? 3 : O.a(U.a(a, 4), b) ? 4 : l
}
function hh() {
  var a = tf(["\ufdd0:ukranian", "\ufdd0:norwegian", "\ufdd0:japanese", "\ufdd0:spaniard", "\ufdd0:englishman"]);
  return J(function c(d) {
    return new W(l, p, function() {
      for(var f = d;;) {
        var h = H(f);
        if(h) {
          var i = h, j = J(i), m = j, n = U.c(m, 0, l), s = U.c(m, 1, l), r = U.c(m, 2, l), v = U.c(m, 3, l), u = U.c(m, 4, l);
          if(O.a("\ufdd0:norwegian", n) && (h = H(function(c, d, f, h, i, j, m, n, r, s) {
            return function kd(v) {
              return new W(l, p, function() {
                for(var u = v;;) {
                  var ia = H(u);
                  if(ia) {
                    var hb = ia, za = J(hb), se = U.c(za, 0, l), te = U.c(za, 1, l), ib = U.c(za, 2, l), Bb = U.c(za, 3, l), Cb = U.c(za, 4, l);
                    if(O.a($(n, Bb) - $(n, Cb), 1) && O.a(ib, "\ufdd0:englishman") && (ia = H(function(c, d, f, h, i, j, m, n, r, s, u, v, E, I, M, P, N, ha, za) {
                      return function kh(ua) {
                        return new W(l, p, function() {
                          for(var ia = ua;;) {
                            var jb = H(ia);
                            if(jb) {
                              var na = jb, Ta = J(na), hb = U.c(Ta, 0, l), ib = U.c(Ta, 1, l), Bb = U.c(Ta, 2, l), Cb = U.c(Ta, 3, l), Ec = U.c(Ta, 4, l);
                              if(O.a("\ufdd0:ukranian", ib) && (O.a(m, Ec) && O.a(U.a(N, 2), hb)) && (jb = H(function(c, d, f, h, i, j, m, n, r, s, u, v, E, I, M, P, N, ha, ia, za, ua, na, Ta, hb, ib, jb, Bb, Cb) {
                                return function lh(Ec) {
                                  return new W(l, p, function() {
                                    for(var bc = Ec;;) {
                                      var cc = H(bc);
                                      if(cc) {
                                        var Kg = cc, Db = J(Kg), Lg = U.c(Db, 0, l), kd = U.c(Db, 1, l), se = U.c(Db, 2, l), te = U.c(Db, 3, l), Mg = U.c(Db, 4, l);
                                        if(O.a(E, Lg) && O.a(Mg, n) && (cc = H(function(a, c, d, f, h, i, j, m, n, r, s, u, v, E, I, M, P, N, ha, ia, za, ua, na, Ta, hb, ib, jb, Bb, Cb, Db, bc, cc, Ec, kd, Q) {
                                          return function Ng(a) {
                                            return new W(l, p, function() {
                                              for(var c = a;;) {
                                                if(c = H(c)) {
                                                  if(Jc(c)) {
                                                    var d = rb(c), f = T(d), h = new Td(Array(f), 0);
                                                    a: {
                                                      for(var r = 0;;) {
                                                        if(r < f) {
                                                          var s = B.a(d, r), u = U.c(s, 0, l), v = U.c(s, 1, l), E = U.c(s, 2, l), P = U.c(s, 3, l), s = U.c(s, 4, l), N = O.a($(Q, j) - $(Q, s), 1);
                                                          if(N ? N : O.a($(Q, j) + $(Q, s), 1)) {
                                                            if((N = O.a($(Q, i) - $(Q, P), 1)) ? N : O.a($(Q, i) + $(Q, P), 1)) {
                                                              if(((N = O.a($(Q, "\ufdd0:norwegian") - $(Q, na), 1)) ? N : O.a($(Q, "\ufdd0:norwegian") + $(Q, na), 1)) && O.a(m, v) && O.a("\ufdd0:spaniard", E) && O.a("\ufdd0:japanese", n)) {
                                                                u = Eg.g(R(["\ufdd0:yellow", ua, "\ufdd0:water", M, "\ufdd0:kools", i, "\ufdd0:fox", s, "\ufdd0:zebra", u, "\ufdd0:snails", v, "\ufdd0:dog", E, "\ufdd0:horse", P, "\ufdd0:fox", s, "\ufdd0:tea", I], 0)), h.add(u)
                                                              }
                                                            }
                                                          }
                                                          r += 1
                                                        }else {
                                                          d = k;
                                                          break a
                                                        }
                                                      }
                                                      d = g
                                                    }
                                                    return d ? $d(h.T(), Ng(sb(c))) : $d(h.T(), l)
                                                  }
                                                  u = J(c);
                                                  h = U.c(u, 0, l);
                                                  d = U.c(u, 1, l);
                                                  f = U.c(u, 2, l);
                                                  r = U.c(u, 3, l);
                                                  u = U.c(u, 4, l);
                                                  if(v = (v = O.a($(Q, j) - $(Q, u), 1)) ? v : O.a($(Q, j) + $(Q, u), 1)) {
                                                    if(v = (v = O.a($(Q, i) - $(Q, r), 1)) ? v : O.a($(Q, i) + $(Q, r), 1)) {
                                                      v = ((v = O.a($(Q, "\ufdd0:norwegian") - $(Q, na), 1)) ? v : O.a($(Q, "\ufdd0:norwegian") + $(Q, na), 1)) && O.a(m, d) && O.a("\ufdd0:spaniard", f) && O.a("\ufdd0:japanese", n)
                                                    }
                                                  }
                                                  if(v) {
                                                    return S(Eg.g(R(["\ufdd0:yellow", ua, "\ufdd0:water", M, "\ufdd0:kools", i, "\ufdd0:fox", u, "\ufdd0:zebra", h, "\ufdd0:snails", d, "\ufdd0:dog", f, "\ufdd0:horse", r, "\ufdd0:fox", u, "\ufdd0:tea", I], 0)), Ng(K(c)))
                                                  }
                                                  c = K(c)
                                                }else {
                                                  return l
                                                }
                                              }
                                            }, l)
                                          }
                                        }(bc, c, d, f, Db, Lg, kd, se, te, Mg, Kg, cc, h, i, j, m, n, r, s, u, v, E, I, M, P, N, ha, ia, za, ua, na, Ta, hb, ib, jb, Bb, Cb)(eh(a))))) {
                                          return de.a(cc, lh(K(bc)))
                                        }
                                        bc = K(bc)
                                      }else {
                                        return l
                                      }
                                    }
                                  }, l)
                                }
                              }(ia, c, d, Ta, hb, ib, Bb, Cb, Ec, na, jb, f, h, i, j, m, n, r, s, u, v, E, I, M, P, N, ha, za)(eh(a))))) {
                                return de.a(jb, kh(K(ia)))
                              }
                              ia = K(ia)
                            }else {
                              return l
                            }
                          }
                        }, l)
                      }
                    }(u, c, za, se, te, ib, Bb, Cb, hb, ia, d, f, h, i, j, m, n, r, s)(eh(a))))) {
                      return de.a(ia, kd(K(u)))
                    }
                    u = K(u)
                  }else {
                    return l
                  }
                }
              }, l)
            }
          }(f, m, n, s, r, v, u, j, i, h)(eh(a))))) {
            return de.a(h, c(K(f)))
          }
          f = K(f)
        }else {
          return l
        }
      }
    }, l)
  }(eh(a)))
}
function ih() {
  ah.g(R(["Solving..."], 0));
  return ah.g(R([hh()], 0))
}
function jh(a) {
  0 < arguments.length && R(Array.prototype.slice.call(arguments, 0), 0);
  return ih.call(this)
}
jh.n = 0;
jh.k = function(a) {
  H(a);
  return ih()
};
jh.g = ih;
xa = jh;
zc.a(xa, Te(2, gh.qb));
