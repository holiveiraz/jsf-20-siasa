package br.gov.caixa.siasa.model.dto;

import java.io.Serializable;

public abstract class CobolBook implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6229924534826049988L;
	private String coErroWblb;
	private String deErroWblb;
	private String coUsuario;
	private String deCertificacao;
	private String coRetorno;
	private String deMensagem;
	private String coSqlcode;

	public abstract String toCICS();

	public abstract void fromCICS(String bigString);

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