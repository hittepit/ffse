package be.hittepit.ffse.parser

import scala.util.parsing.combinator.JavaTokenParsers
import be.hittepit.ffse.model.Command
import be.hittepit.ffse.model.Engine
import be.hittepit.ffse.model.Event
import be.hittepit.ffse.model.State
import be.hittepit.ffse.model.StateType
import be.hittepit.ffse.model.Transition
import be.hittepit.ffse.model.EngineValidationError
import be.hittepit.ffse.model.EngineValidationError

/*
name ::= [a-zA-Z0-9]+

className ::= [a-zA-Z0-9]+

events ::= "events" <name>* "end"

commands ::= "commands" <command>* "end"

command ::= <name> => <className>

states ::= <startState> <endState>* <state>* 

stateBody ::= [<actions>] <transition> <transition>*

actions ::= "actions" "{" <name>* "}"

transition ::= <name> "=>" <name>

startState ::= "start" <name> <stateBody> "end"

endState ::= "finish" <name> [<actions>] "end"

state ::= "state" <name> <stateBody> "end"

version::= "version" \d+(/.\d+)*

engine ::= "engine" <name> <version> [<events>] [<commands>] [<states>] "end"

engines ::= <engine>*

*/

class ParseException(val msg:String) extends Exception(msg)
class ValidationException(val errors:List[EngineValidationError]) extends Exception("Validation errors"){
  override def toString = "Validation errors -->\n"+((""/:errors)((s,e) => s+e.toString()+"\n"))
}

object FfseParser extends JavaTokenParsers {
	val name = """(?!(end|events|commands|actions|start|state|finish|engine)\b)\b\w+\b""".r
	val className = """(?!(end|events|commands|actions|start|state|finish|engine)\b)([a-zA-Z_$][a-zA-Z\d_$]*\.)*[a-zA-Z_$][a-zA-Z\d_$]*""".r
	  
	def commands:Parser[List[Command]] = "commands"~>rep(command)<~"end"
	
	def command:Parser[Command] = name~"=>"~className ^^ {case n~"=>"~c => Command(n,c)}
	
	def events:Parser[List[Event]] = "events"~>rep(name)<~"end" ^^ {case ns => ns.map(Event(_))}
	
	def transition:Parser[Transition] = name~"=>"~name ^^ {case en~"=>"~sn => Transition(en,sn)}
	
	def actions:Parser[List[String]] = "actions"~"{"~rep(name)~"}" ^^ {case "actions"~"{"~ans~"}" => ans}
	
	def stateBody:Parser[(List[String],List[Transition])] = opt(actions)~transition~rep(transition) ^^ {
	  case Some(ans)~t~ts => (ans,t::ts)
	  case None~t~ts => (Nil,t::ts)
	}
	
	def startState:Parser[State] = "start"~>name~stateBody<~"end" ^^ {case n~sb => State(n,sb._1,sb._2,StateType.START)}
	
	def endState:Parser[State] = "finish"~name~opt(actions)~"end" ^^ {
	  case "finish"~n~Some(as)~"end" => State(n,as,Nil,StateType.END)
	  case "finish"~n~None~"end" => State(n,Nil,Nil,StateType.END)
	}
	
	def state:Parser[State] = "state"~name~stateBody~"end" ^^ {case "state"~n~sb~"end" => State(n,sb._1,sb._2,StateType.STATE)}
	
	def version:Parser[String] = "version"~>"""\d+(\.\d+)*""".r
	
	def engine:Parser[Engine] = "engine"~>name~version~events~opt(commands)~startState~rep(state|endState)<~"end" ^^ {
	  case n~v~es~Some(cs)~ss~sts => Engine(n,v,es,cs,ss,sts)
	  case n~v~es~None~ss~sts => Engine(n,v,es,Nil,ss,sts)
	}
	
	def engines:Parser[List[Engine]] = rep(engine) ^^ {case l => l}
	
	def parse(text:String):List[Engine] = {
	  val r = parseAll(engines,text)
	 
	  if(r.successful){
	    val engines = r.get
	    val errors = (List[EngineValidationError]()/:engines)({(errs,e) => e.errors:::errs})
	    if(errors.isEmpty)
	      engines
	    else
	      throw new ValidationException(errors)
	  }else {
	    throw new ParseException("Parse exception: "+r)
	  }
	}
}
