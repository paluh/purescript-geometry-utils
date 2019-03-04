/* global exports */

"use strict";

exports.toTranslationImpl = function(r) {
  return [
    1, 0, r.x,
    0, 1, r.y,
    0, 0, 1
  ];
}

exports.fromTranslationImpl = function(m) {
  return {
    x: m[2],
    y: m[5]
  }
}

exports.rotationImpl = function(radians) {
  return [
    Math.cos(radians), -Math.sin(radians), 0,
    Math.sin(radians), Math.cos(radians), 0,
    0, 0, 1
  ];
};

exports.scaling = function(r) {
  return [
    r.x, 0, 0,
    0, r.y, 0,
    0, 0, 1
  ];
};

exports.translate = function(m) {
  return function(v) {
    var r = m.slice();
    r[2] = m[2] + v.x;
    r[5] = m[5] + v.y;
    return r;
  };
};
