package br.gov.caixa.siasa.model.dto;

import java.util.List;

import org.apache.commons.validator.GenericValidator;
import org.apache.log4j.Logger;

import br.gov.caixa.siasa.exception.InvalidFieldException;

/*
01 ASABK308-AREA.
03 NU-TAMANHO-NEG-BK308                      PIC 9(005).
03 CO-REGRA-NEG-BK308                        PIC X(005).
03 DADOS-CONTROLE-BK308.
   05 CO-RETORNO-WBLB-BK308                  PIC 9(003).
   05 DE-MENSAGEM-WBLB-BK308                 PIC X(061).
   05 FILLER                                 PIC X(021).
03 DADOS-ENTRADA-BK308 REDEFINES DADOS-CONTROLE-BK308.
   05 IC-ACAO-BK308                          PIC X(001).
   05 IC-FASE-BK308                          PIC X(001).
   05 CO-TIPO-CANAL-ENT-BK308                PIC X(003).
   05 DE-TIPO-CANAL-ENT-BK308                PIC X(060).
   05 CO-USUARIO-ENT-BK308                   PIC X(008).
   05 LI-CERTIFICACAO-ENT-BK308              PIC X(012).
03 DADOS-SAIDA-BK308.
   07 NU-OCORRENCIAS-BK308                   PIC 9(003).
   07 TAB-SAI-BK308  OCCURS  30  TIMES.
      10 CO-TIPO-CANAL-BK308                 PIC X(003).
      10 DE-TIPO-CANAL-BK308                 PIC X(060).
03 CODIGOS-DE-RETORNO.
   07 CO-RETORNO-BK308                       PIC 9(001).
   07 DE-MENSAGEM-BK308                      PIC X(080).
   07 CO-ERRO-SQL-BK308                      PIC X(004).
*/
public final class Asabk308DTO extends CobolBook {

	private static final long serialVersionUID = -4511067044320320439L;
	private static final Logger logger = Logger.getLogger(Asabk306DTO.class);

//	ENTRADA
	private String icAcao;
	private String icFase;
	private String coTipoCanal;
	private String deTipoCanal;
//	SAIDA
	private String nuOcorrencia;
	private List<TipoCanal> tipoCanal;
	
	@Override
	public String toCICS() {
		if(GenericValidator.isBlankOrNull(getCoTipoCanal())) {
			setCoTipoCanal("0");
		}
		
		StringBuilder builder = new StringBuilder();
		builder.append(getIcAcao());
		builder.append(getIcFase());
		builder.append(pic(getCoTipoCanal(),3));
		builder.append(pic(getDeTipoCanal(),60));
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
				final String littleString = bigString.substring(beginIndex, beginIndex + 63);
				final TipoCanal tc = new TipoCanal();
				tc.fromCICS(littleString);
				getTipoCanal().add(tc);
				beginIndex += 63;
			}
			beginIndex += (63 * 30) - (63 * x);
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

	public String getIcAcao() {
		return icAcao;
	}

	public void setIcAcao(String icAcao) {
		this.icAcao = icAcao;
	}

	public String getIcFase() {
		return icFase;
	}

	public void setIcFase(String icFase) {
		this.icFase = icFase;
	}

	public String getCoTipoCanal() {
		return coTipoCanal;
	}

	public void setCoTipoCanal(String coTipoCanal) {
		this.coTipoCanal = coTipoCanal;
	}

	public String getDeTipoCanal() {
		return deTipoCanal;
	}

	public void setDeTipoCanal(String deTipoCanal) {
		this.deTipoCanal = deTipoCanal;
	}

	public String getNuOcorrencia() {
		return nuOcorrencia;
	}

	public void setNuOcorrencia(String nuOcorrencia) {
		this.nuOcorrencia = nuOcorrencia;
	}

	public List<TipoCanal> getTipoCanal() {
		return tipoCanal;
	}

	public void setTipoCanal(List<TipoCanal> tipoCanal) {
		this.tipoCanal = tipoCanal;
	}

	public final class TipoCanal {
		private String coTipoCanal;
		private String deTipoCanal;
		
		protected void fromCICS(final String littleString) {
			if (GenericValidator.isBlankOrNull(littleString)) {
				throw new InvalidFieldException("Tripão de retorno inválido: nulo ou vazio");
			}
			try {
				int beginIndex = 0;
				setCoTipoCanal(littleString.substring(beginIndex,beginIndex+3));
				logger.debug("coTipoCanal ["+getCoTipoCanal()+"]");
				beginIndex += 3;
				setDeTipoCanal(littleString.substring(beginIndex,beginIndex+60));
				logger.debug("deTipoCanal ["+getDeTipoCanal()+"]");
				beginIndex += 60;
			} catch (IndexOutOfBoundsException e) {
				logger.error("Tripão inválido: " + littleString, e);
				throw new InvalidFieldException("Tripão devolvido pelo CICS com tamanho inválido", e);
			} catch (NumberFormatException e) {
				logger.error("Tripão inválido: " + littleString, e);
				throw new InvalidFieldException("Tripão devolvido pelo CICS contém dados com valor inesperado",e);
			}
		}

		public final String getCoTipoCanal() {
			return coTipoCanal;
		}

		public void setCoTipoCanal(final String coTipoCanal) {
			this.coTipoCanal = coTipoCanal;
		}

		public final String getDeTipoCanal() {
			return deTipoCanal;
		}

		public void setDeTipoCanal(final String deTipoCanal) {
			this.deTipoCanal = deTipoCanal;
		}
		
	}
}
