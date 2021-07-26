package mylib

import spinal.core._
import spinal.lib._

import scala.language.postfixOps


case class GraphConfig(nodeIdxWidth : Int, nodePrtWidth : Int, adjAddrWidth : Int, adjLenWidth : Int, nodeDataWidth : Int){
  def nodeIdxType = UInt(nodeIdxWidth bits)
  def nodeDataType = UInt(nodeDataWidth bits)
}