enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], value: A, right: Tree[A])
}
