
enum JSON:
  case Seq(elems: List[JSON])
  case Obj(bindings: Map[String, JSON])
  case Num(num: Double)
  case Str(str: String)
  case Bool(b: Boolean)
  case Null

def inQuotes(str: String): String = "\"" + str + "\""

// define the method that return the string representation of JSON data
def show(json: JSON): String = json match
  case JSON.Seq(elems) =>
    elems.map(show).mkString("[",",","]")
  case JSON.Obj(bindings) =>
    bindings.map((s, json) => inQuotes(s) + ":" + show(json)).mkString("{",",\n","}")
  case JSON.Num(num) => num.toString
  case JSON.Str(str) => inQuotes(str)
  case JSON.Bool(b) => b.toString
  case JSON.Null => "null"


val jsData: JSON.Obj = JSON.Obj(Map(
  "firstName" -> JSON.Str("John"),
  "lastName" -> JSON.Str("Smith"),
  "address" -> JSON.Obj(Map(
    "streetAddress" -> JSON.Str("21 2nd Street"),
    "state" -> JSON.Str("NY"),
    "postalCode" -> JSON.Num(10021)
  )),
  "phoneNumbers" -> JSON.Seq(List(
    JSON.Obj(Map(
      "type" -> JSON.Str("home"),
      "number" -> JSON.Str("212 555-1234")
    )),
    JSON.Obj(Map(
      "type" -> JSON.Str("fax"),
      "number" -> JSON.Str("646 555-4567")
    ))
  ))
))

show(jsData)

val areaCode = "212"
val phoneNumbers = for {
  case (key, JSON.Seq(elements)) <- jsData.bindings if key == "phoneNumbers"
  case ("number", JSON.Str(number)) <- elements.flatMap(e => e match
    case JSON.Obj(bindings) => bindings.toList
  )
  if number.startsWith(areaCode)
} yield number

phoneNumbers


