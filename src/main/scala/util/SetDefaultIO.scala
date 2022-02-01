package util

import spinal.core._
import spinal.lib.Stream


trait SetDefaultIO {

  def setDefStream[T <: Data](stream: Stream[T]): Unit ={
    if(stream.isMasterInterface){
      stream.valid := False
      // fixme: zero with given bitwidth >> asBits >> toDataType (complex?)
      stream.payload := U(0, stream.payload.getBitsWidth bits).asBits.toDataType[T](cloneOf(stream.payload))
    } else{
      stream.ready := False
    }
  }

  def setDefStream[T <: Data](stream: Stream[T], isMstrOutside : Boolean): Unit ={
    if(isMstrOutside){
      stream.ready := False
    } else{
      stream.valid := False
      // fixme: zero with given bitwidth >> asBits >> toDataType (complex?)
      stream.payload := U(0, stream.payload.getBitsWidth bits).asBits.toDataType[T](cloneOf(stream.payload))
    }
  }

}
