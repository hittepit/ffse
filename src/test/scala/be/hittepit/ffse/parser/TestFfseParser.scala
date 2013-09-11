package be.hittepit.ffse.parser

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import be.hittepit.ffse.model.Engine
import be.hittepit.ffse.model.Command
import be.hittepit.ffse.model.Event
import be.hittepit.ffse.model.Transition
import be.hittepit.ffse.model.StateType
import be.hittepit.ffse.model.State

class TestFfseParser extends FunSuite with MustMatchers{
	test("version 1.0.0 must return 1.0.0"){
	  val text = "version 1.0.0"
	  val r = FfseParser.parseAll(FfseParser.version,text)
	  r.successful must be(true)
	  r.get must be("1.0.0")
	}
	
	test("version 2 must return 2"){
	  val text = "version 2"
	  val r = FfseParser.parseAll(FfseParser.version,text)
	  r.successful must be(true)
	  r.get must be("2")
	}
  
	test("command must be parsed"){
	  val text = "justDoIt => be.hittepit.DoIt"
	  val r = FfseParser.parseAll(FfseParser.command, text)
	  r.successful must be (true)
	  r.get must be (Command("justDoIt","be.hittepit.DoIt"))
	}
  
	test("commands must return a list of commands"){
		val text = """commands
		  test1 => be.Class1
		  test2 => be.Class2
		  test3 => be.Class3
		  end"""
		val r = FfseParser.parseAll(FfseParser.commands, text);
		r.successful must be (true)
		
		val commands = r.get
		commands must have size(3)
		commands must contain(Command("test1","be.Class1"))
		commands must contain(Command("test2","be.Class2"))
		commands must contain(Command("test3","be.Class3"))
	}
	
	test("commands with empty body returns an empty list"){
		val text = "commands end"
		val r = FfseParser.parseAll(FfseParser.commands, text);
		r.successful must be (true)
		r.get must be (List())
	}
	
	test("events must return a list of events name"){
	  val text = """events test1
	    test2
	    endEvent
	    eventendNear
	    eventEnd
	    end"""
	    
	  val r = FfseParser.parseAll(FfseParser.events,text)
	  r.successful must be(true)
	  val events = r.get
	  events must have size(5)
	  events must contain(Event("test1"))
	  events must contain(Event("test2"))
	  events must contain(Event("endEvent"))
	  events must contain(Event("eventendNear"))
	  events must contain(Event("eventEnd"))
	}
	
	test("events with empty body must return an empty list"){
	  val text = """events 
	    end"""
	    
	  val r = FfseParser.parseAll(FfseParser.events,text)
	  r.successful must be(true)
	  val events = r.get
	  events must be('empty)
	}
	
	test("actions must return a list of action names"){
	  val text = "actions{test1 test2  test3}"
	    
	  val r = FfseParser.parseAll(FfseParser.actions,text)
	  r.successful must be(true)
	  val actions = r.get
	  actions must have size(3)
	  actions must contain("test1")
	  actions must contain("test2")
	  actions must contain("test3")
	}
	
	test("actions with no action must return an empty list"){
	  val text = "actions{}"
	    
	  val r = FfseParser.parseAll(FfseParser.actions,text)
	  r.successful must be(true)
	  r.get must be('empty)
	}
	
	test("stateBody with actions must return a fully initialized StateBody object"){
	  val text = """
	    actions{ test1 test2 test3}
	    event1 => state1
	    event2 => state2
	  """
	  val r = FfseParser.parseAll(FfseParser.stateBody,text)
	  r.successful must be(true)
	  val actions = r.get._1
	  val transitions = r.get._2
	  
	  actions must have size(3)
	  actions must contain("test1")
	  actions must contain("test2")
	  actions must contain("test3")
	  
	  transitions must have size(2)
	  transitions must contain(Transition("event1","state1"))
	  transitions must contain(Transition("event2","state2"))
	}
	
	test("stateBody with no actions must return a fully initialized StateBody object"){
	  val text = """
	    event1 => state1
	    event2 => state2
	  """
	  val r = FfseParser.parseAll(FfseParser.stateBody,text)
	  r.successful must be(true)
	  val actions = r.get._1
	  val transitions = r.get._2
	  
	  actions must be('empty)
	  
	  transitions must have size(2)
	  transitions must contain(Transition("event1","state1"))
	  transitions must contain(Transition("event2","state2"))
	}
	
	test("startState must create a start state fully initialized"){
	  val text = """start first
	    actions{ test1 test2 test3}
	    event1 => state1
	    event2 => state2
	    end
	  """
	  val r = FfseParser.parseAll(FfseParser.startState,text)
	  r.successful must be(true)
	  
	  val s = r.get
	  s.name must be("first")
	  s.stateType must be(StateType.START)
	  val actions = s.actionsName
	  val transitions = s.transitions
	  actions must have size(3)
	  actions must contain("test1")
	  actions must contain("test2")
	  actions must contain("test3")
	  
	  transitions must have size(2)
	  transitions must contain(Transition("event1","state1"))
	  transitions must contain(Transition("event2","state2"))
	}
	
	test("endState must create a end state fully initialized"){
	  val text = """finish last
	    actions{ test1 test2 test3}
	    end
	  """
	  val r = FfseParser.parseAll(FfseParser.endState,text)
	  r.successful must be(true)
	  
	  val s = r.get
	  s.name must be("last")
	  s.stateType must be(StateType.END)
	  val actions = s.actionsName
	  val transitions = s.transitions
	  actions must have size(3)
	  actions must contain("test1")
	  actions must contain("test2")
	  actions must contain("test3")
	  
	  transitions must be('Empty)
	}
	
	test("state must create a state fully initialized"){
	  val text = """state etat1
	    actions{ test1 test2 test3}
	    event1 => state1
	    event2 => state2
	    end
	  """
	  val r = FfseParser.parseAll(FfseParser.state,text)
	  r.successful must be(true)
	  
	  val s = r.get
	  s.name must be("etat1")
	  s.stateType must be(StateType.STATE)
	  val actions = s.actionsName
	  val transitions = s.transitions
	  actions must have size(3)
	  actions must contain("test1")
	  actions must contain("test2")
	  actions must contain("test3")
	  
	  transitions must have size(2)
	  transitions must contain(Transition("event1","state1"))
	  transitions must contain(Transition("event2","state2"))
	}
	
	test("single engine with correct name must be created"){
	  val text = """
	    engine test
			  version 1.0
	    
			  events
			  	e1 
			  	e2 
			  	e3
			  end
	    
			  commands
			  	act1 => be.test.Action1
			  	act2 => be.test.Action2
			  end
	    
			  start begin
			  	e1 => st1
			  end
	    
			  state st1
			  	actions{act1 act2}
			  	e2 => fin
			  	e3 => st2
			  end
	    
			  state st2
			  	e2 => fin
			  end

			  finish fin
			  end
	    end"""
	  val m = FfseParser.parseAll(FfseParser.engine,text)
	  m.successful must be (true)
	  val engine = m.get
	  engine.name must be("test")
	  engine.comment must be(None)
	  engine.version must be("1.0")
	  engine.commands must have size(2)
	  engine.commands must contain(Command("act1","be.test.Action1"))
	  engine.commands must contain(Command("act2","be.test.Action2"))
	  engine.events must have size(3)
	  engine.events must contain(Event("e1"))
	  engine.events must contain(Event("e2"))
	  engine.events must contain(Event("e3"))
	  engine.startState must be(State("begin",Nil,List(Transition("e1","st1")),StateType.START))
	  val states = engine.states
	  states must have size(3)
	  states must contain(State("st1",List("act1","act2"),List(Transition("e2","fin"),Transition("e3","st2")),StateType.STATE))
	  states must contain(State("st2",Nil,List(Transition("e2","fin")),StateType.STATE))
	  states must contain(State("fin",Nil,Nil,StateType.END))
	}
	
	test("single engine with correct name and comment must be created"){
	  val text = """
	    engine test
			  (Ceci est un test élémentaire)
			  version 1.0
	    
			  events
			  	e1 
			  	e2 
			  	e3
			  end
	    
			  commands
			  	act1 => be.test.Action1
			  	act2 => be.test.Action2
			  end
	    
			  start begin
			  	e1 => st1
			  end
	    
			  state st1
			  	actions{act1 act2}
			  	e2 => fin
			  	e3 => st2
			  end
	    
			  state st2
			  	e2 => fin
			  end

			  finish fin
			  end
	    end"""
	  val m = FfseParser.parseAll(FfseParser.engine,text)
	  m.successful must be (true)
	  val engine = m.get
	  engine.name must be("test")
	  engine.comment must be(Some("Ceci est un test élémentaire"))
	  engine.version must be("1.0")
	  engine.commands must have size(2)
	  engine.commands must contain(Command("act1","be.test.Action1"))
	  engine.commands must contain(Command("act2","be.test.Action2"))
	  engine.events must have size(3)
	  engine.events must contain(Event("e1"))
	  engine.events must contain(Event("e2"))
	  engine.events must contain(Event("e3"))
	  engine.startState must be(State("begin",Nil,List(Transition("e1","st1")),StateType.START))
	  val states = engine.states
	  states must have size(3)
	  states must contain(State("st1",List("act1","act2"),List(Transition("e2","fin"),Transition("e3","st2")),StateType.STATE))
	  states must contain(State("st2",Nil,List(Transition("e2","fin")),StateType.STATE))
	  states must contain(State("fin",Nil,Nil,StateType.END))
	}

	test("ParseException must be raised if there is parse exception"){
	  val text = """
	    engine e1
			  version 1.0
	    
			  events
			  	next
			  end
	    
			  start begin
			  	next => stop
			  end
	    
			  finish stop
			  end
	    end
	    
	    engine e2
			  events
			  	next
			  end
	    
			  start begin
			  	next => end
			  end
	    
			  finish end
			  end
	    end
	    """
	  val e = evaluating(FfseParser.parse(text)) must produce[ParseException]
	}

	test("ValidationException must be raised if there is parse is valid, but validation fails"){
	  val text = """
	    engine e1
			  version 1.0
	    
			  events
			  	nexta
			  end
	    
			  start begin
			  	next => stop
			  end
	    
			  finish stop
			  end
	    end
	    
	    engine e2
			  version 2.0
	    
			  events
			  	next
			  end
	    
			  start begin
			  	next => stop
			  end
	    
			  finish stopit
			  end
	    end
	    """
	  val e = evaluating(FfseParser.parse(text)) must produce[ValidationException]
	}
	
	test("execption if engine name duplicated"){}
}