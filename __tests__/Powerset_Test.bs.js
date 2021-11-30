// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest2$AdventOfCode = require("../interop/Jest2.bs.js");
var Powerset$AdventOfCode = require("../src/Powerset.bs.js");

describe("Powerset", (function () {
        var int_array_1 = [
          1,
          2,
          3
        ];
        var int_array_1_result = [
          [],
          [3],
          [2],
          [
            2,
            3
          ],
          [1],
          [
            1,
            3
          ],
          [
            1,
            2
          ],
          [
            1,
            2,
            3
          ]
        ];
        var string_array_1 = [
          "a",
          "b",
          "c"
        ];
        var string_array_1_result = [
          [],
          ["c"],
          ["b"],
          [
            "b",
            "c"
          ],
          ["a"],
          [
            "a",
            "c"
          ],
          [
            "a",
            "b"
          ],
          [
            "a",
            "b",
            "c"
          ]
        ];
        describe("powerset_array_with_list()", (function () {
                var int_tests = [[
                    Powerset$AdventOfCode.powerset_array_with_list(int_array_1),
                    int_array_1_result
                  ]];
                var string_tests = [[
                    Powerset$AdventOfCode.powerset_array_with_list(string_array_1),
                    string_array_1_result
                  ]];
                Jest2$AdventOfCode.testEach2("int", int_tests, (function (result, expected) {
                        expect(result).toEqual(expected);
                        
                      }));
                return Jest2$AdventOfCode.testEach2("string", string_tests, (function (result, expected) {
                              expect(result).toEqual(expected);
                              
                            }));
              }));
        describe("powerset_array()", (function () {
                var int_tests = [[
                    Powerset$AdventOfCode.powerset_array(int_array_1),
                    int_array_1_result
                  ]];
                var string_tests = [[
                    Powerset$AdventOfCode.powerset_array(string_array_1),
                    string_array_1_result
                  ]];
                Jest2$AdventOfCode.testEach2("int", int_tests, (function (result, expected) {
                        expect(result).toEqual(expected);
                        
                      }));
                return Jest2$AdventOfCode.testEach2("string", string_tests, (function (result, expected) {
                              expect(result).toEqual(expected);
                              
                            }));
              }));
        
      }));

/*  Not a pure module */
