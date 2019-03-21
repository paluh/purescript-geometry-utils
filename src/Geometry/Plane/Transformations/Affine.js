/* global exports */

"use strict";

exports.translation = function(r) {
  return [
    1, 0, r.x,
    0, 1, r.y,
    0, 0, 1
  ];
}

exports.toTranslation = function(m) {
  return {
    x: m[2],
    y: m[5]
  }
}

exports.rotation = function(radians) {
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

