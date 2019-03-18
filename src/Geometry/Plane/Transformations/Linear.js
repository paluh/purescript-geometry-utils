/* global exports */

"use strict";

exports.rotationImpl = function(radians) {
  return [
    Math.cos(radians), -Math.sin(radians),
    Math.sin(radians), Math.cos(radians)
  ];
};

exports.scaling = function(r) {
  return [
    r.x, 0,
    0, r.y
  ];
};
