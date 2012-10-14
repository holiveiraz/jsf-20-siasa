package br.gov.caixa.siasa.model.dto;

import java.util.List;

import org.apache.commons.validator.GenericValidator;
import org.apache.log4j.Logger;

import br.gov.caixa.siasa.exception.InvalidFieldException;

/*
01 ASABK307-AREA.
03 NU-TAMANHO-NEG-BK307                      PIC 9(005).
03 CO-REGRA-NEG-BK307                        PIC X(005).
03 DADOS-CONTROLE-BK307.
   05 CO-RETORNO-WBLB-BK307                  PIC 9(003).
   05 DE-MENSAGEM-WBLB-BK307                 PIC X(047).
03 DADOS-ENTRADA-BK307 REDEFINES DADOS-CONTROLE-BK307.
   05 DT-REMESSA-DE-ENT-BK307                PIC X(010).
   05 DT-REMESSA-ATE-ENT-BK307               PIC X(010).
   05 NU-REMESSA-ENT-BK307                   PIC 9(005).
   05 CO-USUARIO-ENT-BK307                   PIC X(008).
   05 LI-CERTIFICACAO-ENT-BK307              PIC X(012).
   05 FILLER                                 PIC X(005).
03 DADOS-SAIDA-BK307.
   07 NU-OCORRENCIAS-BK307                   PIC 9(003).
   07 TAB-SAI-BK307  OCCURS  30  TIMES.
      10 NU-REMESSA-OLD-BK307                PIC 9(005).
      10 NU-REMESSA-BK307                    PIC 9(005).
      10 DT-REMESSA-BK307                    PIC X(010).
      10 HH-REMESSA-BK307                    PIC X(008).
      10 QT-ITEM-PESSOA-FIS-BK307            PIC 9(011).
      10 VR-PESSOA-FIS-BK307                 PIC 9(013)V99.
      10 QT-ITEM-PESSOA-JUR-BK307            PIC 9(011).
      10 VR-PESSOA-JUR-BK307                 PIC 9(013)V99.
      10 QT-ITEM-NAO-IDENT-BK307             PIC 9(011).
      10 VR-NAO-IDENT-BK307                  PIC 9(013)V99.
      10 VR-ESTORNADO-BK307                  PIC 9(013)V99.
   07 CODIGOS-DE-RETORNO.
      10 CO-RETORNO-BK307                    PIC 9(001).
      10 DE-MENSAGEM-BK307                   PIC X(080).
      10 CO-ERRO-SQL-BK307                   PIC X(004).
*/      
public final class Asabk307DTO extends CobolBook {

	private static final long serialVersionUID = 9001368971692207319L;
	private static final Logger logger = Logger.getLogger(Asabk306DTO.class);

//	ENTRADA
	private String dtRemessaDe;
	private String dtRemessaAte;
	private String nuRemessa;
//	SAIDA
	private String nuOcorrencia;
	private List<RemessaRealizada> remessaRealizada;
	
	@Override
	public String toCICS() {
		if(GenericValidator.isBlankOrNull(getDtRemessaDe())) {
			setDtRemessaDe(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtRemessaAte())) {
			setDtRemessaAte(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getNuRemessa())) {
			setNuRemessa("0");
		}
		
		StringBuilder builder = new StringBuilder();
		builder.append(getDtRemessaDe());
		builder.append(getDtRemessaAte());
		builder.append(pic(getNuRemessa(),5));
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
			setNuOcorrencia(bigString.substring(beginIndex, beginIndex+3));
			logger.debug("nuOcorrencia ["+getNuOcorrencia()+"]");
			beginIndex += 3;
			
			logger.debug("===============  L I S T A  ===============");
			int x = Integer.parseInt(getNuOcorrencia());
			for(int i=0; i<x; i++) {
				final String littleString = bigString.substring(beginIndex, beginIndex + 121);
				final RemessaRealizada lancamento = new RemessaRealizada();
				lancamento.fromCICS(littleString);
				getRemessaRealizada().add(lancamento);
				beginIndex += 121;
			}
			beginIndex += (121 * 30) - (121 * x);
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

	public final String getDtRemessaDe() {
		return dtRemessaDe;
	}

	public void setDtRemessaDe(final String dtRemessaDe) {
		this.dtRemessaDe = dtRemessaDe;
	}

	public final String getDtRemessaAte() {
		return dtRemessaAte;
	}

	public void setDtRemessaAte(final String dtRemessaAte) {
		this.dtRemessaAte = dtRemessaAte;
	}

	public final String getNuRemessa() {
		return nuRemessa;
	}

	public void setNuRemessa(final String nuRemessa) {
		this.nuRemessa = nuRemessa;
	}

	public final String getNuOcorrencia() {
		return nuOcorrencia;
	}

	public void setNuOcorrencia(final String nuOcorrencia) {
		this.nuOcorrencia = nuOcorrencia;
	}

	public List<RemessaRealizada> getRemessaRealizada() {
		return remessaRealizada;
	}

	public void setRemessaRealizada(List<RemessaRealizada> remessaRealizada) {
		this.remessaRealizada = remessaRealizada;
	}

	public final class RemessaRealizada {
		private String nuRemessaOld;
		private String nuRemessa;
		private String dtRemessa;
		private String hrRemessa;
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
				setNuRemessaOld(littleString.substring(beginIndex,beginIndex+5));
				logger.debug("nuRemessaOld ["+getNuRemessaOld()+"]");
				beginIndex += 5;
				setNuRemessa(littleString.substring(beginIndex,beginIndex+5));
				logger.debug("nuRemessa ["+getNuRemessa()+"]");
				beginIndex += 5;
				setDtRemessa(littleString.substring(beginIndex,beginIndex+10));
				logger.debug("dtRemessa ["+getDtRemessa()+"]");
				beginIndex += 5;
				setHrRemessa(littleString.substring(beginIndex,beginIndex+8));
				logger.debug("hrRemessa ["+getHrRemessa()+"]");
				beginIndex += 5;
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

		public final String getNuRemessaOld() {
			return nuRemessaOld;
		}

		private void setNuRemessaOld(final String nuRemessaOld) {
			this.nuRemessaOld = nuRemessaOld;
		}

		public final String getNuRemessa() {
			return nuRemessa;
		}

		private void setNuRemessa(final String nuRemessa) {
			this.nuRemessa = nuRemessa;
		}

		public final String getDtRemessa() {
			return dtRemessa;
		}

		private void setDtRemessa(final String dtRemessa) {
			this.dtRemessa = dtRemessa;
		}

		public final String getHrRemessa() {
			return hrRemessa;
		}

		private void setHrRemessa(final String hrRemessa) {
			this.hrRemessa = hrRemessa;
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
