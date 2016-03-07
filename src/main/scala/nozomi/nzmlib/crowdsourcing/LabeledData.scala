package nozomi.nzmlib.crowdsourcing

/**
  * Created by sai on 16-3-7.
  */
case class LabeledData(entity: Double, person: Double, label: Double) {
    override def toString = {
        s"Problem $entity is labeled as $label by persion $person"
    }
}
