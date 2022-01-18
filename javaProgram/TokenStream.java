//package xml;
import java.io.*;// imports it because it throws io exceptions


public interface TokenStream {
    public Token nextToken() throws IOException;
}
 

