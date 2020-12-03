package org.icar.pmr_solver.high_level_specification

/******* PLANNING DOMAIN ********/
case class Domain (id:String, space : Array[DomainPredicate], types: Array[DomainType], axioms : Array[Axiom]) {

	def get_predicate_arg_type(functional:String,pos:Int) : DomainArgument = {
		var t:DomainArgument=NullDomainType()
		for (p<-space)
			if (p.functor==functional && p.args.isDefinedAt(pos))
				t = p.args(pos)
		t
	}

}


/******* DOMAIN TYPES ********/
abstract class DomainType(val name:String) {
	def range : List[ConstantTerm]
}
case class IntegerRange_DomainType(override val name:String, min : Int, max : Int) extends DomainType(name) {
	override def range: List[ConstantTerm] = {
		val numeric_range = (min to max).toList
		for (n <- numeric_range) yield IntegerTerm(n)
	}
}
case class IntegerEnum_DomainType(override val name:String, varrange:List[Int]) extends DomainType(name) {
	override def range: List[ConstantTerm] = {
		for (n <- varrange) yield IntegerTerm(n)
	}
}
case class StringEnum_DomainType(override val name:String, enumer : Array[String]) extends DomainType(name) {
	override def range: List[ConstantTerm] = {
		val array = for (e<-enumer) yield AtomTerm(e)
		array.toList
	}
}



/******* DOMAIN PREDICATES ********/
case class DomainPredicate(functor : String, args : List[DomainArgument])


abstract class DomainArgument {
	def range(types: Array[DomainType]) : List[ConstantTerm]
}


case class DomainVariable(name:String, category : String) extends DomainArgument {
	override def range(types: Array[DomainType]): List[ConstantTerm] =   {
		val tpe = types.find(_.name == category)
		if (tpe.isDefined)
			tpe.get.range
		else
			List.empty
	}
}
case class DomainConstant(name : String) extends DomainArgument {
	override def range(types: Array[DomainType]): List[ConstantTerm] = List(AtomTerm(name))
}
case class DomainConstantString(str : String) extends DomainArgument {
	override def range(types: Array[DomainType]): List[ConstantTerm] = List(StringTerm(str))
}
case class NullDomainType() extends DomainArgument {
	override def range(types: Array[DomainType]) : List[ConstantTerm] = List.empty
}




