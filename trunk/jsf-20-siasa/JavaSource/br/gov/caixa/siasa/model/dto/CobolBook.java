package br.gov.caixa.siasa.model.dto;

import java.io.Serializable;
import java.util.Arrays;

public abstract class CobolBook implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6229924534826049988L;
    private static final char PICX_CHAR = ' ';
    private static final char PIC9_CHAR = '0';
    protected static final char FILLER = ' ';
	protected static final String DATA_ZERADA = "00/00/0000";
    
	private String coErroWblb;
	private String deErroWblb;
	private String coUsuario;
	private String deCertificacao;
	private String coRetorno;
	private String deMensagem;
	private String coSqlcode;

	public abstract String toCICS();

	public abstract void fromCICS(String bigString);

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
    
	public final String getCoErroWblb() {
		return coErroWblb;
	}

	public void setCoErroWblb(final String coErroWblb) {
		this.coErroWblb = coErroWblb;
	}

	public final String getDeErroWblb() {
		return deErroWblb;
	}

	public void setDeErroWblb(final String deErroWblb) {
		this.deErroWblb = deErroWblb;
	}

	public final String getCoUsuario() {
		return coUsuario;
	}

	public void setCoUsuario(final String coUsuario) {
		this.coUsuario = coUsuario;
	}

	public final String getDeCertificacao() {
		return deCertificacao;
	}

	public void setDeCertificacao(final String deCertificacao) {
		this.deCertificacao = deCertificacao;
	}

	public final String getCoRetorno() {
		return coRetorno;
	}

	public void setCoRetorno(final String coRetorno) {
		this.coRetorno = coRetorno;
	}

	public final String getDeMensagem() {
		return deMensagem;
	}

	public void setDeMensagem(final String deMensagem) {
		this.deMensagem = deMensagem;
	}

	public final String getCoSqlcode() {
		return coSqlcode;
	}

	public void setCoSqlcode(final String coSqlcode) {
		this.coSqlcode = coSqlcode;
	}

}