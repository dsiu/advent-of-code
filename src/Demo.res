Js.log("Hello, BuckleScript and Reason and ReScript!");

type color =
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Purple;

let invert = (c: color) =>
  switch (c) {
  | Red => Green
  | Orange => Blue
  | Yellow => Purple
  | Green => Red
  | Blue => Orange
  | Purple => Yellow
  };
