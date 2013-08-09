To be defined

events
  doorClosed
  drawOpened
  lightOn   
  doorOpened
  panelClosed
end

commands
  unlockPanel
  lockPanel
  lockDoor
  unlockDoor
end

state idle
  actions {unlockDoor lockPanel}
  doorClosed => active
end

state active
  drawOpened => waitingForLight
  lightOn    => waitingForDraw
end

state waitingForLight
  lightOn => unlockedPanel
end

state waitingForDraw
  drawOpened => unlockedPanel
end

state unlockedPanel
  actions {unlockPanel lockDoor}
  panelClosed => idle
end


http://blog.efftinge.de/2012/05/implementing-fowlers-state-machine-dsl.html
