import java.nio.Buffer
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Simulation(){
    //Create Elevators and elevatorgroups
    //Create people
    val x=28
    val y=6
    val elevatorXs:Vector[Int] = Vector(3,10,21)
    val placeCoords:Vector[(Int,Int)] = Vector((1, 5), (6, 0), (9, 0), (8, 0), (27, 0), (20, 0), (18, 5), (27, 4), (25,5),(20,5),(12,5), (13,1),(14,1), (15,1), (11,5),(12,5),(13,5), (12,3), (13,3),(14,3))
    val cabinY = Range(1,5)
    val cabinX = Range(0,x).filter(x => !elevatorXs.contains(x))
    val ship:Ship = new Ship(
        elevatorXs.map(x=>new ElevatorGroup(x)),
        placeCoords.map(x=> new Place(x)),
        x,
        y
        ) 
    var people = Vector[Person]()
    for (xx<-cabinX){
        for(yy<-cabinY){
            if(!placeCoords.contains((xx,yy))){
                people = people:+createPerson(new Place((xx,yy)))
            }
        }
    }

    private val randGen = new scala.util.Random;
    var eventHappening: Boolean = false;
    var events: List[(Place, Float)] = List()

    def preference_initializer(cabin:Place,places:Vector[Place]):Map[Place,Map[Place,Float]] = {
        val places_ = places.prepended(cabin)
        var res: List[(Place,Map[Place,Float])]= List()
        for(place1 <- places_){
            var innerMap:List[(Place,Float)] = List()
            for(place2 <- places_){
                if(place1==place2){
                    innerMap = innerMap:+((place2,0.9f))
                }
                else{
                    innerMap = innerMap:+((place2,0.1f))
                }
            }
            res = res :+ (place1,innerMap.toMap)
        }
        res.toMap
    }
    def createPerson(cabin:Place):Person = {
        val preferences = preference_initializer(cabin,ship.places)
        new Person(cabin,preferences,5,ship)
    }
    def update() = {
        people.foreach(_.update(this.events))
        ship.elevatorGroups.foreach(_.update())
    }

    //----custom event------
    val customEventX = 1;
    val customEventFloor = 5;
    var customEventEnabled:Boolean = false;

    def setCustomEvent() = {
        if(!customEventEnabled){
            events = events.appended((ship.places.find(p => p.x == customEventX && p.floor == customEventFloor).get, 100))
            customEventEnabled = true;
        } else {
            events = events.filter(e => e._1.x != customEventX && e._1.floor != customEventFloor)
            customEventEnabled = false;
        }
    }

    def toggleEvent() = {    
        if(eventHappening){
            this.events = List()
            eventHappening = false
            customEventEnabled = false;
        } else {
            val nEvents = 2
            val eventAttractiveness = 100
            val eventPlace = randGen.shuffle(ship.places).filter(e => e.x != customEventX && e.floor != customEventFloor).take(nEvents)
            this.events = this.events :++ (eventPlace zip List.fill(nEvents)(eventAttractiveness))
            eventHappening = true;
        }
    }


    def waitingPeople(): Int = this.people.filter(p => p.state == PersonState.Waiting).length

    def getState():Map[(Int,Int),GridElement] = {
        var items: Vector[((Int, Int), GridElement)] = Vector()
        for(xx<-Range(0,x)){
            for(yy<-Range(0,y)){
                items = items:+((xx,yy),new GridElement)
            }
        }
        val itemMap = items.toMap
        for(person <- people.filter(x=>x.state!=PersonState.InElevator)){
            itemMap((person.xLocation,person.floor)).people+=1
            itemMap((person.ownCabin.x,person.ownCabin.floor)).isCabin=true
        }
        for(eg <- ship.elevatorGroups){
            for(e <- eg.elevators){
                itemMap((eg.xlocation,e.currentFloor)).people+=e.people.size
                itemMap((eg.xlocation,e.currentFloor)).hasElevator=true
            }
            for(yy<-Range(0,y)){
                itemMap((eg.xlocation,yy)).isElevatorShaft=true
            }
        }
        for(place<-ship.places){
            itemMap((place.x,place.floor)).isPlace=true
        }
        itemMap
    }

    override def toString():String = {
        val maxF = y
        val maxX = x
        var s=""
        
        val elevatorGroupX = this.ship.elevatorGroups.map(_.xlocation)
        val placeLocations = this.ship.places.map(p => (p.x, p.floor))
        val peopleLocations = this.people.map(_.xLocation) zip this.people.map(_.floor)
        for (f <- Range(0, maxF).reverse){
            for (x <- Range(0, maxX)){
                if(elevatorGroupX.contains(x)){
                    s+="|"
                } else if (placeLocations.filter(_._2 == f).map(_._1).contains(x)) {
                    s+="P"
                } else if (peopleLocations.filter(_._2 == f).map(_._1).contains(x)) {
                    s+="i"
                } else {
                    s+="_"
                }
            }
            s+="<br>"
        }
        s
    }
}