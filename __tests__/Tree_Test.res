open RescriptCore
open Jest
open Expect
open Jest2
open Tree

describe("Tree", () => {
  // [ [1,2], [3,4] ]
  let tl_1 = Pair(Leaf(1), Leaf(2))
  let tr_1 = Pair(Leaf(3), Leaf(4))
  let tree_1 = Pair(tl_1, tr_1)

  // [ [[1,2], 3], [4, [5,6]] ]

  let tll_2 = Pair(Leaf(1), Leaf(2))
  let tl_2 = Pair(tll_2, Leaf(3))
  let trr_2 = Pair(Leaf(5), Leaf(6))
  let tr_2 = Pair(Leaf(4), trr_2)
  let tree_2 = Pair(tl_2, tr_2)

  let toString = Js.Array.map(((t, r)) => (t->locToString, r->locToString))
  let left_tests =
    [
      (tree_1->top->left, Loc(tl_1, L(Top, tr_1))),
      (tree_2->top->left->left, Loc(tll_2, L(L(Top, tr_2), Leaf(3)))),
    ]->toString

  testEach2("left", left_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  let right_tests =
    [
      (tree_1->top->right, Loc(tr_1, R(tl_1, Top))),
      (tree_2->top->right->right, Loc(trr_2, R(Leaf(4), R(tl_2, Top)))),
    ]->toString

  testEach2("right", right_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  let left_right_tests =
    [
      (tree_1->top->left->right, Loc(Leaf(2), R(Leaf(1), L(Top, tr_1)))),
      (tree_1->top->right->left, Loc(Leaf(3), L(R(tl_1, Top), Leaf(4)))),
      (tree_2->top->left->right, Loc(Leaf(3), R(tll_2, L(Top, tr_2)))),
      (tree_2->top->right->left, Loc(Leaf(4), L(R(tl_2, Top), trr_2))),
    ]->toString

  testEach2("left right", left_right_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  let upmost_tests =
    [
      (tree_1->top->left->right->upmost, Loc(tree_1, Top)),
      (tree_1->top->right->right->upmost, Loc(tree_1, Top)),
      (tree_1->top->left->left->upmost, Loc(tree_1, Top)),
      (tree_1->top->right->left->upmost, Loc(tree_1, Top)),
      (tree_2->top->left->upmost, Loc(tree_2, Top)),
      (tree_2->top->right->upmost, Loc(tree_2, Top)),
      (tree_2->top->left->left->upmost, Loc(tree_2, Top)),
      (tree_2->top->left->right->upmost, Loc(tree_2, Top)),
      (tree_2->top->right->left->upmost, Loc(tree_2, Top)),
      (tree_2->top->right->right->upmost, Loc(tree_2, Top)),
    ]->toString

  testEach2("upmost", upmost_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  let up_tests =
    [
      (tree_1->top->left->right->up, Loc(tl_1, L(Top, tr_1))),
      (tree_1->top->right->left->up, Loc(tr_1, R(tl_1, Top))),
      (tree_2->top->left->left->right->up, Loc(tll_2, L(L(Top, tr_2), Leaf(3)))),
      (tree_2->top->right->left->up, Loc(tr_2, R(tl_2, Top))),
    ]->toString

  testEach2("up", up_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })

  let modify_tests =
    [
      (tree_1->top->left->right->modify(_ => Leaf(5)), Loc(Leaf(5), R(Leaf(1), L(Top, tr_1)))),
      (
        tree_1->top->right->left->modify(_ => Pair(Leaf(6), Leaf(7))),
        Loc(Pair(Leaf(6), Leaf(7)), L(R(tl_1, Top), Leaf(4))),
      ),
      (tree_2->top->left->right->modify(_ => Leaf(9)), Loc(Leaf(9), R(tll_2, L(Top, tr_2)))),
      (
        tree_2->top->right->left->modify(_ => Pair(Leaf(10), Leaf(11))),
        Loc(Pair(Leaf(10), Leaf(11)), L(R(tl_2, Top), trr_2)),
      ),
    ]->toString

  testEach2("modify", modify_tests, (result, expected) => {
    expect(result)->toEqual(expected)
  })
})
