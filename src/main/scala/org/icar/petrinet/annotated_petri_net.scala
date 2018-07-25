package org.icar.petrinet

sealed abstract class PlaceType {
  def short_term : String
}
case class AcceptedState() extends PlaceType {
  override def short_term: String = "a"
}

case class ErrorState() extends PlaceType {
  override def short_term: String = "e"
}

case class WaitAcceptedState() extends PlaceType {
  override def short_term: String = "wa"
}

case class WaitErrorState() extends PlaceType {
  override def short_term: String = "we"
}


case class AnnotatedPlace(id : String, state:PlaceType, maxTokens : Int = 1)
abstract class AnnotatedTransition

case class AnnotatedDirectTransition(dependency:String) extends AnnotatedTransition
case class AnnotatedInverseTransition(dependency:String) extends AnnotatedTransition
case class AnnotatedTruthTransition() extends AnnotatedTransition


abstract class Arc ()
case class PlaceToTransition (place : AnnotatedPlace, transition: AnnotatedTransition) extends Arc
case class TransitionToPlace (transition: AnnotatedTransition,place : AnnotatedPlace) extends Arc


case class PetrinetStructure(places: Array[AnnotatedPlace], transitions: Array[AnnotatedTransition], arcs : Array[Arc])

case class PetrinetDynamic(tokens : scala.collection.mutable.Map[String,Int]) {

  def token_in_place(name : String): Int = {
    if (tokens.contains(name))
      tokens(name)
    else
      0
  }

  override def clone(): PetrinetDynamic = {
    var newtokens = scala.collection.mutable.Map[String,Int]()
    for (t <- tokens.keys)
      if (tokens(t)>0)
        newtokens = newtokens + (t -> tokens(t))

    PetrinetDynamic(newtokens)
  }

  override def toString: String = {
    var string=""
    for (s <- tokens.keys)
      if (tokens(s)>0)
        string += "("+s+")"

    string
  }
}

case class Petrinet(structure: PetrinetStructure, dynamic : PetrinetDynamic) {

  override def toString: String = dynamic.toString

  def get_fireable_transitions:List[AnnotatedTransition] = for (t<-all_transitions if can_fire(t)) yield t
  def all_transitions:List[AnnotatedTransition] = {
    var l = Set[AnnotatedTransition]()

    for (a <- structure.arcs) {
      a match {
        case ptt: PlaceToTransition =>
          l = l + ptt.transition
        case _ =>
      }
    }
    for (a <- structure.arcs) {
      a match {
        case ptt: TransitionToPlace =>
          l = l + ptt.transition
        case _ =>
      }
    }

    l.toList
  }

  def incoming_places(t:AnnotatedTransition): List[AnnotatedPlace] = {
    var l = List[AnnotatedPlace]()

    for (a <- structure.arcs if a.isInstanceOf[PlaceToTransition]) {
      val ptt = a.asInstanceOf[PlaceToTransition]
      if (ptt.transition==t)
        l = ptt.place :: l
    }

    l
  }
  def outgoing_places(t:AnnotatedTransition): List[AnnotatedPlace] = {
    var l = List[AnnotatedPlace]()

    for (a <- structure.arcs if a.isInstanceOf[TransitionToPlace]) {
      val ptt = a.asInstanceOf[TransitionToPlace]
      if (ptt.transition==t)
        l = ptt.place :: l
    }

    l
  }

  def can_fire(t : AnnotatedTransition) : Boolean = all_incoming_places_have_tokens(t) & all_outgoing_places_can_receive_tokens(t)
  def all_incoming_places_have_tokens(t: AnnotatedTransition): Boolean = {
    var b = true
    for (p <- incoming_places(t))
      if (dynamic.token_in_place(p.id)==0 )
        b=false
    b
  }
  def all_outgoing_places_can_receive_tokens(t: AnnotatedTransition): Boolean = {
    var b = true
    for (p <- outgoing_places(t))
      if (dynamic.token_in_place(p.id)>=p.maxTokens )
        b=false
    b
  }

  def fire(t : AnnotatedTransition) : Unit = {
    consume_tokens_in_incoming_places(t)
    produce_tokens_in_outgoing_places(t)
  }

  def consume_tokens_in_incoming_places(t : AnnotatedTransition) : Unit= {
    for (p <- incoming_places(t))
      dynamic.tokens(p.id) = dynamic.token_in_place(p.id)-1
  }

  def produce_tokens_in_outgoing_places(t : AnnotatedTransition): Unit = {
    for (p <- outgoing_places(t))
      dynamic.tokens(p.id) = dynamic.token_in_place(p.id) + 1
  }

  def get_places_with_tokens : List[AnnotatedPlace] = {
    var arr = List[AnnotatedPlace]()
    for (p <- structure.places)
      if (dynamic.token_in_place(p.id)>0)
        arr = p :: arr
    arr
  }

}
