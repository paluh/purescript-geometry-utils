/* global exports */

"use strict";

exports.addImpl = function(m1) {
  return function(m2) {
    return [
      m1[0] + m2[0],
      m1[1] + m2[1],
      m1[2] + m2[2],
      m1[3] + m2[3]
    ]
  }
}

exports.zeroImpl = [
  0, 0,
  0, 0
];

exports.mulImpl = function(m1) {
  return function(m2) {
    return [
      m1[0] * m2[0] + m1[1] * m2[2],
      m1[0] * m2[1] + m1[1] * m2[3],
      m1[2] * m2[0] + m1[3] * m2[2],
      m1[2] * m2[1] + m1[3] * m2[3]
    ];
  };
};

exports.oneImpl = [
  1, 0,
  0, 1
];

exports.subImpl = function(m1) {
  return function(m2) {
    return [
      m1[0] - m2[0],
      m1[1] - m2[1],
      m1[2] - m2[2],
      m1[3] - m2[3]
    ]
  }
}
