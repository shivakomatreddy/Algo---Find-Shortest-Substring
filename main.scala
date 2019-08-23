//Given a string and a list contains certain characters, find the shortest substring containing all the characters in the list.

val string = "The world is here. this is a life full of ups and downs. life is world"
val seqOfWords = Seq("life", "ups", "is", "world")

def getShortestSubstring(string: String, stringOfWords: Seq[String]): String = {
  val substrings = string.replace(". ", "*").split('*').toSeq
  val mapOfWords = seqOfWords.map(wrd => (wrd -> wrd)).toMap
  val result = substrings flatMap { subStr => {
    val list = subStr.split(' ').toSeq.map(str => mapOfWords.get(str))
    if (list.contains(None)) Some(0 -> "")
    else {
      Some(list.size -> list.flatten.mkString(" "))
    }
  }}
  val groupsSubstrings = result.filter(_._1 > 0).groupBy(_._1)
  val shortestSubstrings = groupsSubstrings.keys.toSeq.min

  groupsSubstrings(shortestSubstrings).map(_._2).mkString("\n")
}

println(getShortestSubstring(string, seqOfWords))
