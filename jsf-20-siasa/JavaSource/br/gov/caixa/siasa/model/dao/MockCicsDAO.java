package br.gov.caixa.siasa.model.dao;

import java.util.Arrays;

public abstract class MockCicsDAO {

    private static final char PICX_CHAR = ' ';
    private static final char PIC9_CHAR = '0';
    protected static final char FILLER = ' ';

    protected String pic(final String field, final int x) {

        final StringBuilder builder = new StringBuilder(field);
        final int len = builder.length();

        if (len > x) {
            builder.setLength(x);
        } else {
            final char[] str = new char[x - len];
            Arrays.fill(str, PICX_CHAR);
            builder.insert(len, str);
        }

        return builder.toString();
    }
    //metodos para adicionar 0 a esquerda do numero
    protected String pic(final long field, final int x) {
        final StringBuilder builder = new StringBuilder(Long.toString(field));
        final int len = builder.length();

        if (len > x) {
            builder.setLength(x);
        } else {
            final char[] str = new char[x - len];
            Arrays.fill(str, PIC9_CHAR);
            builder.insert(0, str);
        }

        return builder.toString();
    }
}
