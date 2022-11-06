import java.util.Locale

object PersonState extends Enumeration {
    type PersonState = Value;
    val Walking, Waiting, InElevator, AtLocation = Value
}

object PersonType extends Enumeration {
    type PersonType = Value;
    val Partiers = Value(0)
    val Shoppers = Value(1)
    val Leisurers = Value(2)

    private var rand = new scala.util.Random

    def getRandom() = this.apply(rand.nextInt(this.maxId))
}

class Person(val ownCabin:Place, transitionMatrix:Map[Place,Map[Place,Float]], queueToFloorRatio: Float = 1, ship: Ship, val personType: PersonType.Value = PersonType.getRandom(), attractionMultiplier: Float = 2) {
    

    //first element of map = from, second element of map = to
    var xLocation:Int = ownCabin.x
    var floor:Int = ownCabin.floor
    var state = PersonState.AtLocation
    var goal:Option[Place] = None
    //only has currentlocation if statae = atlocation
    var currentLocation:Option[Place] = Some(ownCabin)
    
    def update(effects:List[(Place,Float)]) = {
        state match{
            case PersonState.AtLocation => getNewGoal(effects)
            case PersonState.Walking    => walk()
            case PersonState.Waiting    => ()
            case PersonState.InElevator => ()
        }
    }
    def getNewGoal(effects: List[(Place,Float)]){
        var transitionMap = transitionMatrix(currentLocation.get)
        transitionMap=transitionMap.map(x => (x._1, if(x._1.appealsTo == this.personType){x._2*attractionMultiplier}else{x._2}))
        for(effect <- effects){
            transitionMap=transitionMap.updated(effect._1,transitionMap(effect._1)*effect._2)
        }
        val transitions = transitionMap.toList
        val sumOfProbs = scala.util.Random.between(0,transitions.map(_._2).sum)
        var s:Float = 0
        var i:Int = 0
        while(s<sumOfProbs){
            s += transitions(i)._2
            i += 1
        }
        val newGoal = transitions(i-1)._1

        if(! (newGoal == currentLocation.get)){
            state = PersonState.Walking
            currentLocation = None
            goal = Some(newGoal)
        }
    }
    def walk(){
        val goal_ = goal.get
        if(goal_.floor==floor){
            gotoX(goal_.x)
            if(xLocation==goal_.x){
                state=PersonState.AtLocation
                currentLocation=goal
                goal = None
            }
        }
        else{
            val elevator = closestElevatorGroup
            gotoX(elevator.xlocation)
            if(xLocation==elevator.xlocation){
                if(stairsWorthIt(elevator)){
                    walkToY(goal.get.floor)
                }
                else{
                    elevator.waitingPeople.addOne(this)
                    state=PersonState.Waiting
                }
            }
        }
    }

    def closestElevatorGroup:ElevatorGroup = ship.elevatorGroups.minBy(x=>math.abs(x.xlocation-xLocation))

    def walkToY(y:Int)={
        if(floor>y) floor-=1
        else floor+=1
    }
    def gotoX(x:Int){
        if(xLocation>x) xLocation -=1
        else if (xLocation<x) xLocation +=1
    }
    def stairsWorthIt(eg: ElevatorGroup):Boolean = {
        val deltaY = math.abs(floor-goal.get.floor)
        if(eg.waitingPeople.size>deltaY*queueToFloorRatio){
            return true
        }
        else false
    }
}