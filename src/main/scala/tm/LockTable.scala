package tm

import spinal.core.Component
import spinal.lib._
import spinal.lib.bus.amba4.axi._

case class LockTableConfig(
                          max_owner_cnt : Int,
                          max_wait_cnt  : Int
                          )

case class LockTable(config:LockTableConfig) extends Component{

}


