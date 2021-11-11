object Step2 {
  def foldList[ELEM, COMB](nil: COMB, cons: (ELEM, COMB) => COMB): List[ELEM] => COMB = {
    case Nil           => nil
    case elem :: listr => cons(elem, foldList(nil, cons)(listr))
  }

  def foldOption[ELEM, COMB](none: COMB, some: ELEM => COMB): Option[ELEM] => COMB = {
    case None       => none
    case Some(elem) => some(elem)
  }

  def foldEither[ELEML, ELEMR, COMB](left: ELEML => COMB, right: ELEMR => COMB): Either[ELEML, ELEMR] => COMB = {
    case Left(eleml)  => left(eleml)
    case Right(elemr) => right(elemr)
  }

  def foldTree[ELEM, COMB](leaf: ELEM => COMB, branch: (COMB, ELEM, COMB) => COMB): Tree[ELEM] => COMB = {
    case Tree.Leaf(elem)                 => leaf(elem)
    case Tree.Branch(treel, elem, treer) => branch(foldTree(leaf, branch)(treel), elem, foldTree(leaf, branch)(treer))
  }

  def foldJson[COMB](jnull: COMB, jboolean: Boolean => COMB, jnumber: Double => COMB, jstring: String => COMB, jarray: List[COMB] => COMB, jobject: List[(String, COMB)] => COMB): Json => COMB = {
    case Json.JNull           => jnull
    case Json.JBoolean(value) => jboolean(value)
    case Json.JNumber(value)  => jnumber(value)
    case Json.JString(value)  => jstring(value)
    case Json.JArray(list)    => jarray(list.map(foldJson(jnull, jboolean, jnumber, jstring, jarray, jobject)))
    case Json.JObject(pairs)  => jobject(pairs.map((k, v) => (k, foldJson(jnull, jboolean, jnumber, jstring, jarray, jobject)(v))))
  }
}
