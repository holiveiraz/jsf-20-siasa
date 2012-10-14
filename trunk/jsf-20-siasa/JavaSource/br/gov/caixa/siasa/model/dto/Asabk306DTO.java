package br.gov.caixa.siasa.model.dto;

import java.util.List;

import org.apache.commons.validator.GenericValidator;
import org.apache.log4j.Logger;

import br.gov.caixa.siasa.exception.InvalidFieldException;

/*
01 ASABK306-AREA.
03 NU-TAMANHO-NEG-BK306                      PIC 9(005).
03 CO-REGRA-NEG-BK306                        PIC X(005).
03 DADOS-CONTROLE-BK306.
   05 CO-RETORNO-WBLB-BK306                  PIC 9(003).
   05 DE-MENSAGEM-WBLB-BK306                 PIC X(047).
03 DADOS-ENTRADA-BK306 REDEFINES DADOS-CONTROLE-BK306.
   05 DT-APURACAO-DE-ENT-BK306               PIC X(010).
   05 DT-APURACAO-ATE-ENT-BK306              PIC X(010).
   05 NU-APURACAO-ENT-BK306                  PIC 9(005).
   05 CO-USUARIO-ENT-BK306                   PIC X(008).
   05 LI-CERTIFICACAO-ENT-BK306              PIC X(012).
   05 FILLER                                 PIC X(005).
03 DADOS-SAIDA-BK306.
   07 VR-SALDO-ANTERIOR-BK306                PIC 9(013)V99.
   07 NU-OCORRENCIAS-BK306                   PIC 9(003).
   07 TAB-SAI-BK306  OCCURS  30  TIMES.
      10 NU-APURACAO-BK306                   PIC 9(005).
      10 NU-APURACAO-BK306-R REDEFINES
         NU-APURACAO-BK306                   PIC X(005).
      10 DT-APURACAO-BK306                   PIC X(010).
      10 HH-APURACAO-BK306                   PIC X(008).
      10 QT-ITEM-PESSOA-FIS-BK306            PIC 9(011).
      10 VR-PESSOA-FIS-BK306                 PIC 9(013)V99.
      10 QT-ITEM-PESSOA-JUR-BK306            PIC 9(011).
      10 VR-PESSOA-JUR-BK306                 PIC 9(013)V99.
      10 QT-ITEM-NAO-IDENT-BK306             PIC 9(011).
      10 VR-NAO-IDENT-BK306                  PIC 9(013)V99.
      10 VR-ESTORNADO-BK306                  PIC 9(013)V99.
   07 CODIGOS-DE-RETORNO.
      10 CO-RETORNO-BK306                    PIC 9(001).
      10 DE-MENSAGEM-BK306                   PIC X(080).
      10 CO-ERRO-SQL-BK306                   PIC X(004).
*/      
public final class Asabk306DTO extends CobolBook {

	private static final long serialVersionUID = 8504054652355311713L;
	private static final Logger logger = Logger.getLogger(Asabk306DTO.class);

//	ENTRADA
	private String dtApuracaoDe;
	private String dtApuracaoAte;
	private String nuApuracao;
//	SAIDA
	private String vrSaldoAnterior;
	private String nuOcorrencia;
	private List<ApuracaoRealizada> apuracaoRealizada;
	
	@Override
	public String toCICS() {
		if(GenericValidator.isBlankOrNull(getDtApuracaoDe())) {
			setDtApuracaoDe(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtApuracaoAte())) {
			setDtApuracaoAte(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getNuApuracao())) {
			setNuApuracao("0");
		}
		
		StringBuilder builder = new StringBuilder();
		builder.append(getDtApuracaoDe());
		builder.append(getDtApuracaoAte());
		builder.append(pic(getNuApuracao(),5));
		builder.append(getCoUsuario());
		builder.append(getDeCertificacao());
		
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
			setVrSaldoAnterior(bigString.substring(beginIndex, beginIndex+15));
			logger.debug("vrSaldoAnterior ["+getVrSaldoAnterior()+"]");
			beginIndex += 15;
			setNuOcorrencia(bigString.substring(beginIndex, beginIndex+3));
			logger.debug("nuOcorrencia ["+getNuOcorrencia()+"]");
			beginIndex += 3;
			
			logger.debug("===============  L I S T A  ===============");
			int x = Integer.parseInt(getNuOcorrencia());
			for(int i=0; i<x; i++) {
				final String littleString = bigString.substring(beginIndex, beginIndex + 116);
				final ApuracaoRealizada lancamento = new ApuracaoRealizada();
				lancamento.fromCICS(littleString);
				getApuracaoRealizada().add(lancamento);
				beginIndex += 116;
			}
			beginIndex += (116 * 30) - (116 * x);
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

	public final String getDtApuracaoDe() {
		return dtApuracaoDe;
	}

	public void setDtApuracaoDe(final String dtApuracaoDe) {
		this.dtApuracaoDe = dtApuracaoDe;
	}

	public final String getDtApuracaoAte() {
		return dtApuracaoAte;
	}

	public void setDtApuracaoAte(final String dtApuracaoAte) {
		this.dtApuracaoAte = dtApuracaoAte;
	}

	public final String getNuApuracao() {
		return nuApuracao;
	}

	public void setNuApuracao(final String nuApuracao) {
		this.nuApuracao = nuApuracao;
	}

	public final String getVrSaldoAnterior() {
		return vrSaldoAnterior;
	}

	public void setVrSaldoAnterior(final String vrSaldoAnterior) {
		this.vrSaldoAnterior = vrSaldoAnterior;
	}

	public final String getNuOcorrencia() {
		return nuOcorrencia;
	}

	public void setNuOcorrencia(final String nuOcorrencia) {
		this.nuOcorrencia = nuOcorrencia;
	}

	public List<ApuracaoRealizada> getApuracaoRealizada() {
		return apuracaoRealizada;
	}

	public void setApuracaoRealizada(List<ApuracaoRealizada> apuracaoRealizada) {
		this.apuracaoRealizada = apuracaoRealizada;
	}

	public final class ApuracaoRealizada {
		private String nuApuracao;
		private String dtApuracao;
		private String hrApuracao;
		private String qtItemPessoaFisica;
		private String vrItemPessoaFisica;
		private String qtItemPessoaJuridica;
		private String vrItemPessoaJuridica;
		private String qtItemNaoIdentificado;
		private String vrItemNaoIdentificado;
		private String vrEstornado;
		
		protected void fromCICS(final String littleString) {
			if (GenericValidator.isBlankOrNull(littleString)) {
				throw new InvalidFieldException("Tripão de retorno inválido: nulo ou vazio");
			}
			try {
				int beginIndex = 0;
				setNuApuracao(littleString.substring(beginIndex,beginIndex+5));
				logger.debug("nuApuracao ["+getNuApuracao()+"]");
				beginIndex += 5;
				setDtApuracao(littleString.substring(beginIndex,beginIndex+10));
				logger.debug("dtApuracao ["+getDtApuracao()+"]");
				beginIndex += 10;
				setHrApuracao(littleString.substring(beginIndex,beginIndex+8));
				logger.debug("hrApuracao ["+getHrApuracao()+"]");
				beginIndex += 8;
				setQtItemPessoaFisica(littleString.substring(beginIndex,beginIndex+11));
				logger.debug("qtItemPessoaFisica ["+getQtItemPessoaFisica()+"]");
				beginIndex += 11;
				setVrItemPessoaFisica(littleString.substring(beginIndex,beginIndex+15));
				logger.debug("vrItemPessoaFisica ["+getVrItemPessoaFisica()+"]");
				beginIndex += 15;
				setQtItemPessoaJuridica(littleString.substring(beginIndex,beginIndex+11));
				logger.debug("qtItemPessoaJuridica ["+getQtItemPessoaJuridica()+"]");
				beginIndex += 11;
				setVrItemPessoaJuridica(littleString.substring(beginIndex,beginIndex+15));
				logger.debug("vrItemPessoaJuridica ["+getVrItemPessoaJuridica()+"]");
				beginIndex += 15;
				setQtItemNaoIdentificado(littleString.substring(beginIndex,beginIndex+11));
				logger.debug("qtItemNaoIdentificado ["+getQtItemNaoIdentificado()+"]");
				beginIndex += 11;
				setVrItemNaoIdentificado(littleString.substring(beginIndex,beginIndex+15));
				logger.debug("vrItemNaoIdentificado ["+getVrItemNaoIdentificado()+"]");
				beginIndex += 15;
				setVrEstornado(littleString.substring(beginIndex,beginIndex+15));
				logger.debug("vrEstornado ["+getVrEstornado()+"]");
				beginIndex += 15;
			} catch (IndexOutOfBoundsException e) {
				logger.error("Tripão inválido: " + littleString, e);
				throw new InvalidFieldException("Tripão devolvido pelo CICS com tamanho inválido", e);
			} catch (NumberFormatException e) {
				logger.error("Tripão inválido: " + littleString, e);
				throw new InvalidFieldException("Tripão devolvido pelo CICS contém dados com valor inesperado",e);
			}
		}

		public final String getNuApuracao() {
			return nuApuracao;
		}

		private void setNuApuracao(final String nuApuracao) {
			this.nuApuracao = nuApuracao;
		}

		public final String getDtApuracao() {
			return dtApuracao;
		}

		private void setDtApuracao(final String dtApuracao) {
			this.dtApuracao = dtApuracao;
		}

		public final String getHrApuracao() {
			return hrApuracao;
		}

		private void setHrApuracao(final String hrApuracao) {
			this.hrApuracao = hrApuracao;
		}

		public final String getQtItemPessoaFisica() {
			return qtItemPessoaFisica;
		}

		private void setQtItemPessoaFisica(final String qtItemPessoaFisica) {
			this.qtItemPessoaFisica = qtItemPessoaFisica;
		}

		public final String getVrItemPessoaFisica() {
			return vrItemPessoaFisica;
		}

		private void setVrItemPessoaFisica(final String vrItemPessoaFisica) {
			this.vrItemPessoaFisica = vrItemPessoaFisica;
		}

		public final String getQtItemPessoaJuridica() {
			return qtItemPessoaJuridica;
		}

		private void setQtItemPessoaJuridica(final String qtItemPessoaJuridica) {
			this.qtItemPessoaJuridica = qtItemPessoaJuridica;
		}

		public final String getVrItemPessoaJuridica() {
			return vrItemPessoaJuridica;
		}

		private void setVrItemPessoaJuridica(final String vrItemPessoaJuridica) {
			this.vrItemPessoaJuridica = vrItemPessoaJuridica;
		}

		public final String getQtItemNaoIdentificado() {
			return qtItemNaoIdentificado;
		}

		private void setQtItemNaoIdentificado(final String qtItemNaoIdentificado) {
			this.qtItemNaoIdentificado = qtItemNaoIdentificado;
		}

		public final String getVrItemNaoIdentificado() {
			return vrItemNaoIdentificado;
		}

		private void setVrItemNaoIdentificado(final String vrItemNaoIdentificado) {
			this.vrItemNaoIdentificado = vrItemNaoIdentificado;
		}

		public final String getVrEstornado() {
			return vrEstornado;
		}

		private void setVrEstornado(final String vrEstornado) {
			this.vrEstornado = vrEstornado;
		}
	}
}
