package be.hittepit.ffse.parser

object test {
  val name = """\b(?!end)\b[a-zA-Z0-9]+""".r      //> name  : scala.util.matching.Regex = \b(?!end)\b[a-zA-Z0-9]+
 
 val text = "end test1 test2"                     //> text  : java.lang.String = end test1 test2
 
 val s = name.findFirstMatchIn(text)              //> s  : Option[scala.util.matching.Regex.Match] = Some(test1)
	
	val t = """\w""".r.split(text)            //> t  : Array[String] = Array("", "", "", " ", "", "", "", "", " ")
	
	val dateP1 = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
                                                  //> dateP1  : scala.util.matching.Regex = (\d\d\d\d)-(\d\d)-(\d\d)
	val dateP1(year, month, day) = "2011-07-15"
                                                  //> year  : String = 2011
                                                  //| month  : String = 07
                                                  //| day  : String = 15
}