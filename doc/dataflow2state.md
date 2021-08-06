# Dataflow description
## Category of Memory Object
* List

* Queue


## Category of Access Type





# State machine description
## ReadCmd
* read [data] from [queue]
isActive:
	normal(axiReadCmd.addr: connect)
	criteria(len[queue]!=0)(axiReadCmd.valid: true)
	jump(axiReadCmd.fire){
		(readLock+)
		goto(next)
	}

## ReadResp
* read [data] from [queue]
isActive:
	normal()
	criteria(fire){
		(queueRdAddr++)
		(readLock-)		
	}
	jump(fire){
		goto(next)
	}

## Process




## WriteCmd, WriteData
* write [data] to [queue]
isActive:
	


## WriteRsp



