package com.scalaAsm.portableExe

import java.io.DataOutputStream
import java.io.ByteArrayOutputStream

case class Extern(dllName: String, functionNames: List[String])

case class Imports(val externs: Seq[Extern], val nonExterns: Seq[Extern], val offset: Int) extends ExeWriter {
  
      case class BoundImportDescriptor(val RVAImportLookupTable: Int,
	      val timeStamp: Int,
	      val forwarderChain: Int,
	      val name: Int,
	      val firstThunk: Int) extends ExeWriter {
	
	      def write(stream: DataOutputStream) {
	        write(stream, RVAImportLookupTable)
	        write(stream, timeStamp)
	        write(stream, forwarderChain)
	        write(stream, name)
	        write(stream, firstThunk)
	      }
	    }
      
      def link: CompiledImports = {

        val imports = externs ++ nonExterns
        
        def padNames(name: String) = if (name.size % 2 == 0) Array[Byte](0x00, 0x00) else Array[Byte](0x00)
  
        val flattenedFcns = imports.flatMap(imp => imp.functionNames ++ List(imp.dllName))
        
        def getDllNames(x:Seq[Extern]): List[String] = x.flatMap(x => List(x.dllName)).toList
        def getFunctionNames(x:Seq[Extern]): List[String] = x.flatMap(_.functionNames).toList
        
        def getBoundImports(): (List[BoundImportDescriptor], List[Int]) = {
          
            val initalLookupTableRVA: Map[String, Int] = Map.empty
          
            
	        val nameRVAs: Map[String, Int] = {
              var position = 0
              flattenedFcns.foldLeft(initalLookupTableRVA)((prev, name) => {
                 val skipHint = if (getDllNames(imports).contains(name)) 0 else 2
		          val result = prev ++ Map(name -> (position + skipHint))
		          position += name.length + padNames(name).length + skipHint  //plus two for the hint
		          result
		        })
            }
	         
	      
	        val importFunctionNames = getFunctionNames(imports)
	        val sizeOfAddrTable = importFunctionNames.size*4 + getDllNames(imports).size*4
	        
	        
	        val lookupTableRVAs: Map[String, Int] = {
	          var position = 0
		           imports.foldLeft(initalLookupTableRVA)((offset, dll) => {
		          
		          val result = offset ++ Map(dll.dllName -> position)
		          position += dll.functionNames.size*4 + 4 //plus four for the 4 bytes padding
		          result
		        })
	        }
	        
	        val boundImports = getDllNames(imports).map { dllName => 
	
	          val lookupAddr = offset + lookupTableRVAs(dllName) + (imports.size+1)*20 // assumes imports.size is the number of tables
	          
	          BoundImportDescriptor(RVAImportLookupTable = lookupAddr,
			      timeStamp = 0x00000000,
			      forwarderChain = 0x00000000,
			      name = offset + nameRVAs(dllName) + sizeOfAddrTable*2 + (imports.size+1)*20, // plus 2 to skip the hint
			      firstThunk = lookupAddr + sizeOfAddrTable)
	        } ++ List(BoundImportDescriptor(0,0,0,0,0)) // terminating descriptor
	        
	        def toAddressTable(name: String) = if (getDllNames(imports).contains(name)) 0 else (offset + nameRVAs(name) + (imports.size+1)*20 + sizeOfAddrTable*2 - 2)

	        val importAddressTable = flattenedFcns.map(toAddressTable).toList
	        
	        return (boundImports, importAddressTable)
        }
        
        val (boundImportDescriptors, importAddressTable) = getBoundImports()
        val importNameTable =  	importAddressTable	
        
        val byteOutput = new ByteArrayOutputStream()
        val stream = new DataOutputStream(byteOutput)
        
        boundImportDescriptors.foreach(_.write(stream))
        importNameTable.foreach(x => write(stream, x)) // (INT)
        importAddressTable.foreach(x => write(stream, x)) // (IAT)
        
        flattenedFcns.foreach { name =>
          	if (!getDllNames(imports).contains(name))
	          write(stream, 0.toShort)
	        stream.write(name.toCharArray().map(_.toByte))
	        stream.write(padNames(name))
        }
        
        def getFunctionMap(externList: Seq[Extern]) = {
          val functionNames = getFunctionNames(externList)
          flattenedFcns.zipWithIndex.filter{case (name,_) => functionNames.contains(name)}.map{ case (fcnName, index) => 
                       (fcnName, offset + (imports.size+1)*20 + flattenedFcns.size*4 + index*4)}.toMap
        }
                       
        CompiledImports(byteOutput.toByteArray(), boundImportDescriptors.size*20, importAddressTable.size*4, getFunctionMap(nonExterns), getFunctionMap(externs))
    }
  }