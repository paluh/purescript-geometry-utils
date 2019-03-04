/* global exports */

"use strict";

exports.affineTransform = function(m) {
  return function(p) {
    return {
      x: m[0] * p.x + m[1] * p.y + m[2],
      y: m[3] * p.x + m[4] * p.y + m[5]
    };
  }
}

exports.linearTransform = function(m) {
  return function(p) {
    return {
      x: m[0] * p.x + m[1] * p.y,
      y: m[2] * p.x + m[3] * p.y
    };
  }
}
