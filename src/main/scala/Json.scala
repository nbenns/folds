enum Json {
  case JNull
  case JBoolean(value: Boolean)
  case JNumber(value: Double)
  case JString(value: String)
  case JArray(value: List[Json])
  case JObject(pairs: List[(String, Json)])
}
