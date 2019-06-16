/* global exports */

"use strict";

exports.rotation = function(radians) {
  return [
    Math.cos(radians), -1.0 * Math.sin(radians),
    Math.sin(radians), Math.cos(radians)
  ];
};

exports.scaling = function(r) {
  return [
    r.x, 0,
    0, r.y
  ];
};
