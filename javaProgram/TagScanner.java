//package xml;

import java.io.*;

class TagScanner implements TokenStream {
    public static final int BEGIN_TAG_TYPE = 1;
    public static final int END_TAG_TYPE = 2;
    public static final int TEXT_TYPE = 3;

    protected Reader reader = null;
    /** Lookahead char */
    protected char c;

    /** Text of currently matched token */
    protected StringBuffer text = new StringBuffer(100);

    public TagScanner(Reader reader) throws IOException {
        this.reader = reader;
        nextChar();
    }

    protected void nextChar() throws IOException {
        c = (char)reader.read();
    }

    public Token nextToken() throws IOException {
        text.setLength(0); // reset text
        int type = Token.INVALID_TYPE;
        if ( c=='<' ) {
            text.append(c);
            nextChar();
            type = BEGIN_TAG_TYPE;
            if ( c=='/' ) {
                type = END_TAG_TYPE;
            }
            // scarf until end of tag
            while ( c!=-1 && c!='>' ) {
                text.append(c);
                nextChar();
            }
            text.append(c);
            nextChar();
        }
        else if ( c==(char)-1 ) {
            type = Token.EOF_TYPE;
            text.append("<EOF>");
        }
        else {
            // scarf until start of a tag
            type = TEXT_TYPE;
            while ( c!=(char)-1 && c!='<' ) {
                text.append(c);
                nextChar();
            }
        }
        if ( text.toString().trim().length()==0 ) {
            // try again (tail recursion is like a loop)
            return nextToken();
        }
        type = getTagType(type, text.toString());
        return new Token(type, text.toString());
    }

    public int getTagType(int tagType, String tagText) {
        return tagType; // don't alter
    }
}
