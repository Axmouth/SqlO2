!function(e){function n(n){for(var r,i,f=n[0],s=n[1],c=n[2],a=0,_=[];a<f.length;a++)i=f[a],Object.prototype.hasOwnProperty.call(o,i)&&o[i]&&_.push(o[i][0]),o[i]=0;for(r in s)Object.prototype.hasOwnProperty.call(s,r)&&(e[r]=s[r]);for(b&&b(n);_.length;)_.shift()();return u.push.apply(u,c||[]),t()}function t(){for(var e,n=0;n<u.length;n++){for(var t=u[n],r=!0,i=1;i<t.length;i++)0!==o[t[i]]&&(r=!1);r&&(u.splice(n--,1),e=s(s.s=t[0]))}return e}var r={},o={0:0},u=[],i={},f={sD4X:function(){return{"./postgrustql_wasm_client_bg.js":{__wbindgen_object_drop_ref:function(e){return r.vG0L.exports.s(e)},__wbindgen_number_new:function(e){return r.vG0L.exports.q(e)},__wbindgen_string_new:function(e,n){return r.vG0L.exports.t(e,n)},__wbg_now_1a2bf048df058d4a:function(e){return r.vG0L.exports.j(e)},__wbg_get_fa38f22e54fe1ab1:function(e,n){return r.vG0L.exports.d(e,n)},__wbg_call_20c04382b27a4486:function(e,n){return r.vG0L.exports.a(e,n)},__wbindgen_object_clone_ref:function(e){return r.vG0L.exports.r(e)},__wbg_new_a938277eeb06668d:function(){return r.vG0L.exports.g()},__wbg_push_2bfc5fcfa4d4389d:function(e,n){return r.vG0L.exports.k(e,n)},__wbg_newnoargs_bfddd41728ab0b9c:function(e,n){return r.vG0L.exports.i(e,n)},__wbg_new_bd7709fb051ded5c:function(){return r.vG0L.exports.h()},__wbg_set_c784587ca3fcfb04:function(e,n,t){return r.vG0L.exports.m(e,n,t)},__wbg_entries_632a8997f63284c0:function(e){return r.vG0L.exports.b(e)},__wbg_fromEntries_e999cc8689f8489f:function(e){return r.vG0L.exports.c(e)},__wbg_self_944d201f31e01c91:function(){return r.vG0L.exports.l()},__wbg_window_993fd51731b86960:function(){return r.vG0L.exports.n()},__wbg_globalThis_8379563d70fab135:function(){return r.vG0L.exports.e()},__wbg_global_073eb4249a3a8c12:function(){return r.vG0L.exports.f()},__wbindgen_is_undefined:function(e){return r.vG0L.exports.p(e)},__wbindgen_debug_string:function(e,n){return r.vG0L.exports.o(e,n)},__wbindgen_throw:function(e,n){return r.vG0L.exports.u(e,n)}}}}};function s(n){if(r[n])return r[n].exports;var t=r[n]={i:n,l:!1,exports:{}};return e[n].call(t.exports,t,t.exports,s),t.l=!0,t.exports}s.e=function(e){var n=[],t=o[e];if(0!==t)if(t)n.push(t[2]);else{var r=new Promise((function(n,r){t=o[e]=[n,r]}));n.push(t[2]=r);var u,c=document.createElement("script");c.charset="utf-8",c.timeout=120,s.nc&&c.setAttribute("nonce",s.nc),c.src=function(e){return s.p+""+({}[e]||e)+"-es5."+{5:"48cb5bcae834f7fb3d3c"}[e]+".js"}(e);var a=new Error;u=function(n){c.onerror=c.onload=null,clearTimeout(_);var t=o[e];if(0!==t){if(t){var r=n&&("load"===n.type?"missing":n.type),u=n&&n.target&&n.target.src;a.message="Loading chunk "+e+" failed.\n("+r+": "+u+")",a.name="ChunkLoadError",a.type=r,a.request=u,t[1](a)}o[e]=void 0}};var _=setTimeout((function(){u({type:"timeout",target:c})}),12e4);c.onerror=c.onload=u,document.head.appendChild(c)}return({5:["sD4X"]}[e]||[]).forEach((function(e){var t=i[e];if(t)n.push(t);else{var r,o=f[e](),u=fetch(s.p+""+{sD4X:"be6a752b546fb477b663"}[e]+".module.wasm");r=o instanceof Promise&&"function"==typeof WebAssembly.compileStreaming?Promise.all([WebAssembly.compileStreaming(u),o]).then((function(e){return WebAssembly.instantiate(e[0],e[1])})):"function"==typeof WebAssembly.instantiateStreaming?WebAssembly.instantiateStreaming(u,o):u.then((function(e){return e.arrayBuffer()})).then((function(e){return WebAssembly.instantiate(e,o)})),n.push(i[e]=r.then((function(n){return s.w[e]=(n.instance||n).exports})))}})),Promise.all(n)},s.m=e,s.c=r,s.d=function(e,n,t){s.o(e,n)||Object.defineProperty(e,n,{enumerable:!0,get:t})},s.r=function(e){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},s.t=function(e,n){if(1&n&&(e=s(e)),8&n)return e;if(4&n&&"object"==typeof e&&e&&e.__esModule)return e;var t=Object.create(null);if(s.r(t),Object.defineProperty(t,"default",{enumerable:!0,value:e}),2&n&&"string"!=typeof e)for(var r in e)s.d(t,r,(function(n){return e[n]}).bind(null,r));return t},s.n=function(e){var n=e&&e.__esModule?function(){return e.default}:function(){return e};return s.d(n,"a",n),n},s.o=function(e,n){return Object.prototype.hasOwnProperty.call(e,n)},s.p="https://axmouth.github.io/postgrustql/",s.oe=function(e){throw console.error(e),e},s.w={};var c=window.webpackJsonp=window.webpackJsonp||[],a=c.push.bind(c);c.push=n,c=c.slice();for(var _=0;_<c.length;_++)n(c[_]);var b=a;t()}([]);