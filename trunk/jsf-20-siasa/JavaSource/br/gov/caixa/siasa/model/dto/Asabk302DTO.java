package br.gov.caixa.siasa.model.dto;

import org.apache.commons.validator.GenericValidator;
import org.apache.log4j.Logger;

import br.gov.caixa.siasa.exception.InvalidFieldException;

/*
01 ASABK302-AREA.
03 NU-TAMANHO-NEG-BK302                      PIC 9(005).
03 CO-REGRA-NEG-BK302                        PIC X(005).
03 DADOS-CONTROLE-BK302.
   05 CO-RETORNO-WBLB-BK302                  PIC 9(003).
   05 DE-MENSAGEM-WBLB-BK302                 PIC X(040).
   05 FILLER                                 PIC X(074).
03 DADOS-ENTRADA-BK302 REDEFINES DADOS-CONTROLE-BK302.
   05 CO-USUARIO-ENT-BK302                   PIC X(008).
   05 LI-CERTIFICACAO-ENT-BK302              PIC X(012).
   05 DT-DOACAO-ENT-BK302                    PIC X(010).
   05 DT-COMPENSACAO-ENT-BK302               PIC X(010).
   05 DT-APURACAO-ENT-BK302                  PIC X(010).
   05 DT-PROCESSAMENTO-ENT-BK302             PIC X(010).
   05 NU-DOACAO-ENT-BK302                    PIC 9(009).
   05 VR-DOACAO-ENT-BK302                    PIC 9(013)V9(02).
   05 QT-DIA-BLOQUEIO-ENT-BK302              PIC 9(004).
   05 DT-DE-REMETER-ENT-BK302                PIC X(010).
   05 VR-A-ESTORNAR-ENT-BK302                PIC 9(013)V9(02).
   05 NU-AGENCIA-ENT-BK302                   PIC 9(004).
03 DADOS-SAIDA-BK302 REDEFINES DADOS-CONTROLE-BK302.
   05 RESERVADO-WBLB-SAI-BK302               PIC X(043).
   05 VR-TOTAL-ESTORNADO-SAI-BK302           PIC 9(013)V9(02).
   05 VR-SALDO-REMANESC-SAI-BK302            PIC 9(013)V9(02).
   05 FILLER                                 PIC X(044).
03 CODIGOS-DE-RETORNO.
   05 CO-RETORNO-BK302                       PIC 9(001).
   05 DE-MENSAGEM-BK302                      PIC X(080).
   05 CO-ERRO-SQL-BK302                      PIC X(004).
*/   
public final class Asabk302DTO extends CobolBook {

	private static final long serialVersionUID = 7345394043273798657L;
	private static final Logger logger = Logger.getLogger(Asabk302DTO.class);
//	ENTRADA
	private String dtDoacao;
	private String dtCompensacao;
	private String dtApuracao;
	private String dtProcessamento;
	private String nuDoacao;
	private String vrDoacao;
	private String qtDiaBloqueio;
	private String dtDeRemeter;
	private String vrAEstornar;
	private String nuAgencia;
//	SAIDA
	private String vrTotalEstornado;
	private String vrSaldoRemanescente;
	
	@Override
	public String toCICS() {
		if(GenericValidator.isBlankOrNull(getDtApuracao())) {
			setDtApuracao(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtCompensacao())) {
			setDtCompensacao(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtDeRemeter())) {
			setDtDeRemeter(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtDoacao())) {
			setDtDoacao(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtProcessamento())) {
			setDtProcessamento(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getNuAgencia())) {
			setNuAgencia("0");
		}
		if(GenericValidator.isBlankOrNull(getNuDoacao())) {
			setNuDoacao("0");
		}
		if(GenericValidator.isBlankOrNull(getQtDiaBloqueio())) {
			setQtDiaBloqueio("0");
		}
		if(GenericValidator.isBlankOrNull(getVrAEstornar())) {
			setVrAEstornar("0");
		}
		if(GenericValidator.isBlankOrNull(getVrDoacao())) {
			setVrDoacao("0");
		}
		
		StringBuilder builder = new StringBuilder();
		builder.append(getCoUsuario());
		builder.append(getDeCertificacao());
		builder.append(getDtDoacao());
		builder.append(getDtCompensacao());
		builder.append(getDtApuracao());
		builder.append(getDtProcessamento());
		builder.append(pic(Long.parseLong(getNuDoacao()),9));
		builder.append(pic(Long.parseLong(getVrDoacao()),15));
		builder.append(pic(Long.parseLong(getQtDiaBloqueio()),4));
		builder.append(getDtDeRemeter());
		builder.append(pic(Long.parseLong(getVrAEstornar()),15));
		builder.append(pic(Long.parseLong(getNuAgencia()),4));
		
		logger.debug("ENTRADA: ["+ builder.toString() +"]");
		return builder.toString();
	}

	@Override
	public void fromCICS(String bigString) {
		if (GenericValidator.isBlankOrNull(bigString)) {
			throw new InvalidFieldException("Tripão de retorno inválido: nulo ou vazio");
		}
		try {
			int beginIndex = 43;
			setVrTotalEstornado(bigString.substring(beginIndex, beginIndex+15));
			logger.debug("vrTotalEstornado ["+getVrTotalEstornado()+"]");
			beginIndex += 15;
			setVrSaldoRemanescente(bigString.substring(beginIndex, beginIndex+15));
			logger.debug("vrSaldoRemanescente ["+getVrSaldoRemanescente()+"]");
			beginIndex += 15;
			setCoRetorno(bigString.substring(beginIndex, beginIndex+1));
			logger.debug("coRetorno ["+getCoRetorno()+"]");
			beginIndex += 1;
			setDeMensagem(bigString.substring(beginIndex, beginIndex+80));
			logger.debug("deMensagem ["+getDeMensagem()+"]");
			beginIndex += 80;
			setCoSqlcode(bigString.substring(beginIndex, beginIndex+4));
			logger.debug("coSqlcode ["+getCoSqlcode()+"]");
			beginIndex += 4;
			
		} catch (IndexOutOfBoundsException e) {
			logger.error("Tripão inválido: " + bigString, e);
			throw new InvalidFieldException("Tripão devolvido pelo CICS com tamanho inválido", e);
		} catch (NumberFormatException e) {
			logger.error("Tripão inválido: " + bigString, e);
			throw new InvalidFieldException("Tripão devolvido pelo CICS contém dados com valor inesperado",e);
		}
	}

	public final String getDtDoacao() {
		return dtDoacao;
	}

	public void setDtDoacao(final String dtDoacao) {
		this.dtDoacao = dtDoacao;
	}

	public final String getDtCompensacao() {
		return dtCompensacao;
	}

	public void setDtCompensacao(final String dtCompensacao) {
		this.dtCompensacao = dtCompensacao;
	}

	public final String getDtApuracao() {
		return dtApuracao;
	}

	public void setDtApuracao(final String dtApuracao) {
		this.dtApuracao = dtApuracao;
	}

	public final String getDtProcessamento() {
		return dtProcessamento;
	}

	public void setDtProcessamento(final String dtProcessamento) {
		this.dtProcessamento = dtProcessamento;
	}

	public final String getNuDoacao() {
		return nuDoacao;
	}

	public void setNuDoacao(final String nuDoacao) {
		this.nuDoacao = nuDoacao;
	}

	public final String getVrDoacao() {
		return vrDoacao;
	}

	public void setVrDoacao(final String vrDoacao) {
		this.vrDoacao = vrDoacao;
	}

	public final String getQtDiaBloqueio() {
		return qtDiaBloqueio;
	}

	public void setQtDiaBloqueio(final String qtDiaBloqueio) {
		this.qtDiaBloqueio = qtDiaBloqueio;
	}

	public final String getDtDeRemeter() {
		return dtDeRemeter;
	}

	public void setDtDeRemeter(final String dtDeRemeter) {
		this.dtDeRemeter = dtDeRemeter;
	}

	public final String getVrAEstornar() {
		return vrAEstornar;
	}

	public void setVrAEstornar(final String vrAEstornar) {
		this.vrAEstornar = vrAEstornar;
	}

	public final String getNuAgencia() {
		return nuAgencia;
	}

	public void setNuAgencia(final String nuAgencia) {
		this.nuAgencia = nuAgencia;
	}

	public final String getVrTotalEstornado() {
		return vrTotalEstornado;
	}

	public void setVrTotalEstornado(final String vrTotalEstornado) {
		this.vrTotalEstornado = vrTotalEstornado;
	}

	public final String getVrSaldoRemanescente() {
		return vrSaldoRemanescente;
	}

	public void setVrSaldoRemanescente(final String vrSaldoRemanescente) {
		this.vrSaldoRemanescente = vrSaldoRemanescente;
	}

}
