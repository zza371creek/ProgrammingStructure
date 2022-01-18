//package xml;

import java.io.*;
import java.net.*;

class Tags {
    public static void main(String[] args) throws IOException {		
		File inputFile = new File("project1.xml");//gets the input file
        FileReader in = new FileReader(inputFile);// turns the input file into a stream
        //TagScanner scanner = new TagScanner(new InputStreamReader(system.in));// pass the stream to the program
       // URL yahoo = new URL("http://www.yahoo.com/");
   
		//DataInputStream in = new DataInputStream(
                  //yahoo.openStream());
	    		in.read();		
		TagScanner scanner = new TagScanner(in);
				Token t = scanner.nextToken();
        while ( t.getType()!=Token.EOF_TYPE ) {
            System.out.println(t);
            t = scanner.nextToken();							
        }
    }		
}
