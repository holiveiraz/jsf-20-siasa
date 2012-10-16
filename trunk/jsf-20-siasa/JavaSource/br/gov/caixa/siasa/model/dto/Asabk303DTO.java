package br.gov.caixa.siasa.model.dto;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.validator.GenericValidator;
import org.apache.log4j.Logger;

import br.gov.caixa.siasa.exception.InvalidFieldException;

/*
01 ASABK303-AREA.
03 NU-TAMANHO-NEG-BK303                      PIC 9(005).
03 CO-REGRA-NEG-BK303                        PIC X(005).
03 DADOS-CONTROLE-BK303.
   05 CO-RETORNO-WBLB-BK303                  PIC 9(003).
   05 DE-MENSAGEM-WBLB-BK303                 PIC X(040).
03 DADOS-ENTRADA-BK303 REDEFINES DADOS-CONTROLE-BK303.
   05 CO-USUARIO-ENT-BK303                   PIC X(008).
   05 LI-CERTIFICACAO-ENT-BK303              PIC X(012).
   05 FILLER                                 PIC X(023).
03 DADOS-SAIDA-BK303.
   05 NU-OCORRENCIAS-BK303                   PIC 9(003).
   05 TAB-SAI-BK303  OCCURS  30  TIMES.
      07 NU-DOACAO-BK303                     PIC 9(012).
      07 CO-ESTORNO-BK303                    PIC 9(009).
      07 DT-DOACAO-BK303                     PIC X(010).
      07 DT-REMESSA-BK303                    PIC X(010).
      07 DT-PROCESSAMENTO-BK303              PIC X(010).
      07 VR-ESTORNADO-BK303                  PIC 9(013)V9(02).
      07 CO-USUARIO-BK303                    PIC X(008).
      07 NU-AGENCIA-BK303                    PIC 9(004).
   05 CODIGOS-DE-RETORNO.
      07 CO-RETORNO-BK303                    PIC 9(001).
      07 DE-MENSAGEM-BK303                   PIC X(080).
      07 CO-ERRO-SQL-BK303                   PIC X(004).
*/      
public final class Asabk303DTO extends CobolBook {

	private static final long serialVersionUID = 3130640998401004645L;
	private static final Logger logger = Logger.getLogger(Asabk303DTO.class);
//	SAIDA
	private String nuOcorrencia;
	private List<EstornoPendente> estornoPendente;

	public Asabk303DTO() {
		estornoPendente = new ArrayList<EstornoPendente>();
	}
	
	public String getNuOcorrencia() {
		return nuOcorrencia;
	}

	public void setNuOcorrencia(String nuOcorrencia) {
		this.nuOcorrencia = nuOcorrencia;
	}

	public List<EstornoPendente> getEstornoPendente() {
		return estornoPendente;
	}

	public void setEstornoPendente(List<EstornoPendente> estornoPendente) {
		this.estornoPendente = estornoPendente;
	}

	@Override
	public String toCICS() {
		StringBuilder builder = new StringBuilder();
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
				final String littleString = bigString.substring(beginIndex, beginIndex + 78);
				final EstornoPendente estorno = new EstornoPendente();
				estorno.fromCICS(littleString);
				getEstornoPendente().add(estorno);
				beginIndex += 78;
			}
			beginIndex += (78 * 30) - (78 * x);
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

	public final class EstornoPendente {
		private String nuDoacao;
		private String coEstorno;
		private String dtDoacao;
		private String dtRemessa;
		private String dtProcessamento;
		private String vrEstornado;
		private String coUsuario;
		private String nuAgencia;
		
		public final String getNuDoacao() {
			return nuDoacao;
		}
		private void setNuDoacao(final String nuDoacao) {
			this.nuDoacao = nuDoacao;
		}
		public final String getCoEstorno() {
			return coEstorno;
		}
		private void setCoEstorno(final String coEstorno) {
			this.coEstorno = coEstorno;
		}
		public final String getDtDoacao() {
			return dtDoacao;
		}
		private void setDtDoacao(final String dtDoacao) {
			this.dtDoacao = dtDoacao;
		}
		public final String getDtRemessa() {
			return dtRemessa;
		}
		private void setDtRemessa(final String dtRemessa) {
			this.dtRemessa = dtRemessa;
		}
		public final String getDtProcessamento() {
			return dtProcessamento;
		}
		private void setDtProcessamento(final String dtProcessamento) {
			this.dtProcessamento = dtProcessamento;
		}
		public final String getVrEstornado() {
			return vrEstornado;
		}
		private void setVrEstornado(final String vrEstornado) {
			this.vrEstornado = vrEstornado;
		}
		public final String getCoUsuario() {
			return coUsuario;
		}
		private void setCoUsuario(final String coUsuario) {
			this.coUsuario = coUsuario;
		}
		public final String getNuAgencia() {
			return nuAgencia;
		}
		private void setNuAgencia(final String nuAgencia) {
			this.nuAgencia = nuAgencia;
		}
		protected void fromCICS(final String littleString) {
			if (GenericValidator.isBlankOrNull(littleString)) {
				throw new InvalidFieldException("Tripão de retorno inválido: nulo ou vazio");
			}
			try {
				int beginIndex = 0;
				setNuDoacao(littleString.substring(beginIndex, beginIndex + 12));
				logger.debug("nuDoacao ["+getNuDoacao()+"]");
				beginIndex += 12;
				setCoEstorno(littleString.substring(beginIndex, beginIndex + 9));
				logger.debug("nuCPF ["+getCoEstorno()+"]");
				beginIndex += 9;
				setDtDoacao(littleString.substring(beginIndex, beginIndex + 10));
				logger.debug("dtDoacao ["+getDtDoacao()+"]");
				beginIndex += 10;
				setDtRemessa(littleString.substring(beginIndex,beginIndex + 10));
				logger.debug("dtCompensacao ["+getDtRemessa()+"]");
				beginIndex += 10;
				setDtProcessamento(littleString.substring(beginIndex,beginIndex + 10));
				logger.debug("dtProcessamento ["+getDtProcessamento()+"]");
				beginIndex += 10;
				setVrEstornado(littleString.substring(beginIndex, beginIndex + 15));
				logger.debug("vrEstornado ["+getVrEstornado()+"]");
				beginIndex += 15;
				setCoUsuario(littleString.substring(beginIndex,beginIndex + 8));
				logger.debug("coUsuario ["+getCoUsuario()+"]");
				beginIndex += 8;
				setNuAgencia(littleString.substring(beginIndex, beginIndex + 4));
				logger.debug("nuAgencia ["+getNuAgencia()+"]");
				beginIndex += 4;
			} catch (IndexOutOfBoundsException e) {
				logger.error("Tripão inválido: " + littleString, e);
				throw new InvalidFieldException("Tripão devolvido pelo CICS com tamanho inválido", e);
			} catch (NumberFormatException e) {
				logger.error("Tripão inválido: " + littleString, e);
				throw new InvalidFieldException("Tripão devolvido pelo CICS contém dados com valor inesperado",e);
			}
		}
	}
}
