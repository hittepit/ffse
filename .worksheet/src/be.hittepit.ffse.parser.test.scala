package be.hittepit.ffse.parser

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(91); 
  val name = """\b(?!end)\b[a-zA-Z0-9]+""".r;System.out.println("""name  : scala.util.matching.Regex = """ + $show(name ));$skip(32); 
 
 val text = "end test1 test2";System.out.println("""text  : java.lang.String = """ + $show(text ));$skip(39); 
 
 val s = name.findFirstMatchIn(text);System.out.println("""s  : Option[scala.util.matching.Regex.Match] = """ + $show(s ));$skip(34); 
	
	val t = """\w""".r.split(text);System.out.println("""t  : Array[String] = """ + $show(t ));$skip(49); 
	
	val dateP1 = """(\d\d\d\d)-(\d\d)-(\d\d)""".r;System.out.println("""dateP1  : scala.util.matching.Regex = """ + $show(dateP1 ));$skip(45); 
	val dateP1(year, month, day) = "2011-07-15";System.out.println("""year  : String = """ + $show(year ));System.out.println("""month  : String = """ + $show(month ));System.out.println("""day  : String = """ + $show(day ))}
}