class ControlFunction {
    
    def respond(input: String): String = {
        val (opcode, paramMap) = CommandParser(input)
        if( opcode == "React" ) {
            val generation = paramMap("generation").toInt
            val time = paramMap("time").toInt
            val energy = paramMap("energy").toInt
            val view = View( paramMap("view") )
            val previousDirectionStr = if (paramMap.contains("previousDirection")) paramMap("previousDirection") else "1:1"
            val previousDirection = Pos.parse(previousDirectionStr)
            if( generation == 0 ) {
                
                if (paramMap.contains("forcedDirection")) {
                    val dir = Pos.parse(paramMap("forcedDirection"))
                    if (view.isFree(dir)) {
                        val stepsLeft = paramMap("stepsLeft").toInt
                        if (stepsLeft > 0) {
                            return "Move(direction=" + dir + ")" +
                                "|Set(stepsLeft=" + (stepsLeft - 1) + ")" +
                                "|Set(forcedDirection=" + dir + ")" +
                                "|Set(previousDirection=" + dir + ")"
                        }
                    }
                }
                
                
                val f = view.firstFood
                if (f != None) {
                    val dir = view.directionTowardsPos(f.get)
                    if (view.isFree(dir)) {
                        "Status(text=Found food!)|Move(direction=" + dir + ")" +
                        "|Set(previousDirection=" + dir + ")"
                    } else {
                        if (view.isEnemy(dir)) {
                            val inv = dir.invertedDirection
                            if (!view.isEnemy(inv)) {
                                "Say(text=BACK OUT)" +
                                "|Status(text=ENEMY)" +
                                "|Move(direction=" + inv + ")" +
                                "|Set(forcedDirection=" + inv + ")" +
                                "|Set(stepsLeft=5)" +
                                "|Set(previousDirection=" + inv + ")"
                            } else {
                                val twisted = view.getFreeDirection(dir)
                                "Say(text=OUCH)" +
                                "|Status(text=ENEMY)" +
                                "|Move(direction=" + twisted + ")" +
                                "|Set(forcedDirection=" + twisted + ")" +
                                "|Set(stepsLeft=5)" +
                                "|Set(previousDirection=" + twisted + ")"
                            }
                        } else {
                            val twisted = view.getFreeDirection(dir)
                            "Say(text=back out)" +
                            "|Status(text=Wall)" +
                            "|Move(direction=" + twisted + ")" +
                            "|Set(forcedDirection=" + twisted + ")" +
                            "|Set(stepsLeft=5)" +
                            "|Set(previousDirection=" + twisted + ")"
                        }
                    }
                } else {
                    val twisted = view.getFreeDirection(previousDirection)
                    "Status(text=Scouting)" +
                    "|Move(direction=" + twisted + ")" +
                    "|Set(forcedDirection=" + twisted + ")" +
                    "|Set(stepsLeft=5)" +
                    "|Set(previousDirection=" + twisted + ")"
                }
            } else "" 
        } else ""
    }
}

case class Pos(x:Int, y:Int) {
    def direction = Pos( if(x==0) 0 else x/math.abs(x), if(y==0) 0 else y/math.abs(y) )
    def invertedDirection = Pos(y, x)
    override def toString = x+":"+y
}

object Pos {
    def parse(in:String) = {
    	val v = in.split(':').map(_.toInt)
		Pos(v(0),v(1))
	}
}

case class View(data:String) {
    val dimension = math.sqrt(data.length).toInt
	val delta = ((dimension-1) / 2)

	def firstFood:Option[Pos] = {
		val food = data.view.zipWithIndex.filter(p => p._1 == 'P' || p._1 == 'B')
		if (food.isEmpty)
			None
		else
			Some( food.map(p => indexToPos(p._2)).minBy( distanceToPos(_) ) )
	}

    def firstFree:Option[Pos] = {
		val free = data.view.zipWithIndex.filter(p => p._1 == '_' || p._1 == 'P' || p._1 == 'B')
		if (free.isEmpty)
			None
		else
			Some( free.map(p => indexToPos(p._2)).minBy( distanceToPos(_) ) )
	}
    
    def getFreeDirection(currentDirection:Pos) = {
        this.firstFree match {
            case Some(p) => this.directionTowardsPos(p)
            case None => currentDirection.invertedDirection
        }
    }

    def firstEnemy:Option[Pos] = {
		val food = data.view.zipWithIndex.filter(p => p._1 == 'b' || p._1 == 's')
		if (food.isEmpty)
			None
		else
			Some( food.map(p => indexToPos(p._2)).minBy( distanceToPos(_) ) )
	}

    def apply(pos:Pos) = data( posToIndex(pos) )
    def isFree(pos:Pos) = "BP_".contains(this.apply(pos))
    def isEnemy(pos:Pos) = "bs".contains(this.apply(pos))
	def posToIndex(p:Pos) = (p.y+delta)*dimension + delta + p.x
	def indexToPos(i:Int):Pos = Pos(i%dimension-delta, i/dimension-delta)
	def distanceToPos(pos:Pos) = (math.abs(pos.x)+math.abs(pos.y)).toInt
    def directionTowardsPos(pos:Pos) = {
        val x = if (pos.x == 0) 0 else math.abs(pos.x)/pos.x
        val y = if (pos.y == 0) 0 else math.abs(pos.y)/pos.y
        Pos(x, y)
    }
}

object CommandParser {
    def apply(command: String) = {
        def splitParam(param: String) = {
            val segments = param.split('=')
            if( segments.length != 2 )
                throw new IllegalStateException("invalid key/value pair: " + param)
            (segments(0),segments(1))
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)

        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map( splitParam ).toMap
        (segments(0), keyValuePairs)
    }
}

class ControlFunctionFactory {
    def create = new ControlFunction().respond _
}

