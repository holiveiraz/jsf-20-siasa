package br.gov.caixa.siasa.model.dto;

import java.util.List;

import org.apache.commons.validator.GenericValidator;
import org.apache.log4j.Logger;

import br.gov.caixa.siasa.exception.InvalidFieldException;

/*
01 ASABK305-AREA.
03 NU-TAMANHO-NEG-BK305                      PIC 9(005).
03 CO-REGRA-NEG-BK305                        PIC X(005).
03 DADOS-CONTROLE-BK305.
   05 CO-RETORNO-WBLB-BK305                  PIC 9(003).
   05 DE-MENSAGEM-WBLB-BK305                 PIC X(077).
03 DADOS-ENTRADA-BK305 REDEFINES DADOS-CONTROLE-BK305.
   05 DT-DOACAO-ENT-DE-BK305                 PIC X(010).
   05 DT-DOACAO-ENT-ATE-BK305                PIC X(010).
   05 DT-COMPENSACAO-ENT-DE-BK305            PIC X(010).
   05 DT-COMPENSACAO-ENT-ATE-BK305           PIC X(010).
   05 DT-REMESSA-ENT-DE-BK305                PIC X(010).
   05 DT-REMESSA-ENT-ATE-BK305               PIC X(010).
   05 CO-USUARIO-ENT-BK305                   PIC X(008).
   05 LI-CERTIFICACAO-ENT-BK305              PIC X(012).
03 DADOS-SAIDA-BK305.
   05 NU-OCORRENCIAS-BK305                   PIC 9(003).
   05 TAB-SAI-BK305  OCCURS  90  TIMES.
      07 NU-DOACAO-BK305                     PIC 9(012).
      07 NU-CPF-BK305                        PIC 9(011).
      07 NU-CNPJ-BK305                       PIC 9(014).
      07 DT-DOACAO-BK305                     PIC X(010).
      07 DT-COMPENSACAO-BK305                PIC X(010).
      07 DT-REMESSA-BK305                    PIC X(010).
      07 DT-INCLUSAO-EST-BK305               PIC X(010).
      07 CO-FINANCEIRO-BK305                 PIC 9(009).
      07 QT-DIA-BLOQUEIO-BK305               PIC 9(004).
      07 DT-ESTORNO-BK305                    PIC X(010).
      07 VR-DOACAO-BK305                     PIC 9(013)V9(02).
      07 VR-ESTORNADO-BK305                  PIC 9(013)V9(02).
      07 VR-SALDO-BK305                      PIC 9(013)V9(02).
      07 NU-AGENCIA-BK305                    PIC 9(004).
   05 CODIGOS-DE-RETORNO.
      07 CO-RETORNO-BK305                    PIC 9(001).
      07 DE-MENSAGEM-BK305                   PIC X(080).
      07 CO-ERRO-SQL-BK305                   PIC X(004).
*/
public final class Asabk305DTO extends CobolBook {

	private static final long serialVersionUID = 4693323717476647370L;
	private static final Logger logger = Logger.getLogger(Asabk305DTO.class);
//	ENTRADA
	private String dtDoacaoDe;
	private String dtDoacaoAte;
	private String dtCompensacaoDe;
	private String dtCompensacaoAte;
	private String dtRemessaDe;
	private String dtRemessaAte;
//	SAIDA
	private String nuOcorrencia;
	private List<LancamentoCheque> lancamentoCheque;

	@Override
	public String toCICS() {
		if(GenericValidator.isBlankOrNull(getDtCompensacaoDe())) {
			setDtCompensacaoDe(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtCompensacaoAte())) {
			setDtCompensacaoAte(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtRemessaDe())) {
			setDtRemessaDe(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtRemessaAte())) {
			setDtRemessaAte(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtDoacaoDe())) {
			setDtDoacaoDe(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtDoacaoAte())) {
			setDtDoacaoAte(DATA_ZERADA);
		}
		
		StringBuilder builder = new StringBuilder();
		builder.append(getDtDoacaoDe());
		builder.append(getDtDoacaoAte());
		builder.append(getDtCompensacaoDe());
		builder.append(getDtCompensacaoAte());
		builder.append(getDtRemessaDe());
		builder.append(getDtRemessaAte());
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
			beginIndex =+ 3;
			
			logger.debug("===============  L I S T A  ===============");
			int x = Integer.parseInt(getNuOcorrencia());
			for(int i=0; i<x; i++) {
				final String littleString = bigString.substring(beginIndex, beginIndex + 149);
				final LancamentoCheque lancamento = new LancamentoCheque();
				lancamento.fromCICS(littleString);
				getLancamentoCheque().add(lancamento);
				beginIndex += 149;
			}
			beginIndex += (149 * 90) - (149 * x);
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

	public final String getDtDoacaoDe() {
		return dtDoacaoDe;
	}

	public void setDtDoacaoDe(final String dtDoacaoDe) {
		this.dtDoacaoDe = dtDoacaoDe;
	}

	public final String getDtDoacaoAte() {
		return dtDoacaoAte;
	}

	public void setDtDoacaoAte(final String dtDoacaoAte) {
		this.dtDoacaoAte = dtDoacaoAte;
	}

	public final String getDtCompensacaoDe() {
		return dtCompensacaoDe;
	}

	public void setDtCompensacaoDe(final String dtCompensacaoDe) {
		this.dtCompensacaoDe = dtCompensacaoDe;
	}

	public final String getDtCompensacaoAte() {
		return dtCompensacaoAte;
	}

	public void setDtCompensacaoAte(final String dtCompensacaoAte) {
		this.dtCompensacaoAte = dtCompensacaoAte;
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

	public final String getNuOcorrencia() {
		return nuOcorrencia;
	}

	public void setNuOcorrencia(final String nuOcorrencia) {
		this.nuOcorrencia = nuOcorrencia;
	}

	public List<LancamentoCheque> getLancamentoCheque() {
		return lancamentoCheque;
	}

	public void setLancamentoCheque(List<LancamentoCheque> lancamentoCheque) {
		this.lancamentoCheque = lancamentoCheque;
	}
	
	public final class LancamentoCheque {
		private String nuDoacao;
		private String nuCPF;
		private String nuCNPJ;
		private String dtDoacao;
		private String dtCompensacao;
		private String dtRemessa;
		private String dtInclusaoEstorno;
		private String coFinanceiro;
		private String qtDiaBloqueio;
		private String dtEstorno;
		private String vrDoacao;
		private String vrEstornado;
		private String vrSaldo;
		private String nuAgencia;
		
		protected void fromCICS(final String littleString) {
			if (GenericValidator.isBlankOrNull(littleString)) {
				throw new InvalidFieldException("Tripão de retorno inválido: nulo ou vazio");
			}
			try {
				int beginIndex = 0;
				setNuDoacao(littleString.substring(beginIndex,beginIndex+12));
				logger.debug("nuDoacao ["+getNuDoacao()+"]");
				beginIndex =+ 12;
				setNuCPF(littleString.substring(beginIndex,beginIndex+11));
				logger.debug("nuDoacao ["+getNuCPF()+"]");
				beginIndex =+ 11;
				setNuCNPJ(littleString.substring(beginIndex,beginIndex+14));
				logger.debug("nuDoacao ["+getNuCNPJ()+"]");
				beginIndex =+ 14;
				setDtDoacao(littleString.substring(beginIndex,beginIndex+10));
				logger.debug("nuDoacao ["+getDtDoacao()+"]");
				beginIndex =+ 10;
				setDtCompensacao(littleString.substring(beginIndex,beginIndex+10));
				logger.debug("nuDoacao ["+getDtCompensacao()+"]");
				beginIndex =+ 10;
				setDtRemessa(littleString.substring(beginIndex,beginIndex+10));
				logger.debug("nuDoacao ["+getDtRemessa()+"]");
				beginIndex =+ 10;
				setDtInclusaoEstorno(littleString.substring(beginIndex,beginIndex+10));
				logger.debug("nuDoacao ["+getDtInclusaoEstorno()+"]");
				beginIndex =+ 10;
				setCoFinanceiro(littleString.substring(beginIndex,beginIndex+9));
				logger.debug("nuDoacao ["+getCoFinanceiro()+"]");
				beginIndex =+ 9;
				setQtDiaBloqueio(littleString.substring(beginIndex,beginIndex+4));
				logger.debug("nuDoacao ["+getQtDiaBloqueio()+"]");
				beginIndex =+ 4;
				setDtEstorno(littleString.substring(beginIndex,beginIndex+10));
				logger.debug("nuDoacao ["+getDtEstorno()+"]");
				beginIndex =+ 10;
				setVrDoacao(littleString.substring(beginIndex,beginIndex+15));
				logger.debug("nuDoacao ["+getVrDoacao()+"]");
				beginIndex =+ 15;
				setVrEstornado(littleString.substring(beginIndex,beginIndex+15));
				logger.debug("nuDoacao ["+getVrEstornado()+"]");
				beginIndex =+ 15;
				setVrSaldo(littleString.substring(beginIndex,beginIndex+15));
				logger.debug("nuDoacao ["+getVrSaldo()+"]");
				beginIndex =+ 15;
				setNuAgencia(littleString.substring(beginIndex,beginIndex+4));
				logger.debug("nuDoacao ["+getNuAgencia()+"]");
				beginIndex =+ 4;
			} catch (IndexOutOfBoundsException e) {
				logger.error("Tripão inválido: " + littleString, e);
				throw new InvalidFieldException("Tripão devolvido pelo CICS com tamanho inválido", e);
			} catch (NumberFormatException e) {
				logger.error("Tripão inválido: " + littleString, e);
				throw new InvalidFieldException("Tripão devolvido pelo CICS contém dados com valor inesperado",e);
			}
		}

		public final String getNuDoacao() {
			return nuDoacao;
		}

		private void setNuDoacao(final String nuDoacao) {
			this.nuDoacao = nuDoacao;
		}

		public final String getNuCPF() {
			return nuCPF;
		}

		private void setNuCPF(final String nuCPF) {
			this.nuCPF = nuCPF;
		}

		public final String getNuCNPJ() {
			return nuCNPJ;
		}

		private void setNuCNPJ(final String nuCNPJ) {
			this.nuCNPJ = nuCNPJ;
		}

		public final String getDtDoacao() {
			return dtDoacao;
		}

		private void setDtDoacao(final String dtDoacao) {
			this.dtDoacao = dtDoacao;
		}

		public final String getDtCompensacao() {
			return dtCompensacao;
		}

		private void setDtCompensacao(final String dtCompensacao) {
			this.dtCompensacao = dtCompensacao;
		}

		public final String getDtRemessa() {
			return dtRemessa;
		}

		private void setDtRemessa(final String dtRemessa) {
			this.dtRemessa = dtRemessa;
		}

		public final String getDtInclusaoEstorno() {
			return dtInclusaoEstorno;
		}

		private void setDtInclusaoEstorno(final String dtInclusaoEstorno) {
			this.dtInclusaoEstorno = dtInclusaoEstorno;
		}

		public final String getCoFinanceiro() {
			return coFinanceiro;
		}

		private void setCoFinanceiro(final String coFinanceiro) {
			this.coFinanceiro = coFinanceiro;
		}

		public final String getQtDiaBloqueio() {
			return qtDiaBloqueio;
		}

		private void setQtDiaBloqueio(final String qtDiaBloqueio) {
			this.qtDiaBloqueio = qtDiaBloqueio;
		}

		public final String getDtEstorno() {
			return dtEstorno;
		}

		private void setDtEstorno(final String dtEstorno) {
			this.dtEstorno = dtEstorno;
		}

		public final String getVrDoacao() {
			return vrDoacao;
		}

		private void setVrDoacao(final String vrDoacao) {
			this.vrDoacao = vrDoacao;
		}

		public final String getVrEstornado() {
			return vrEstornado;
		}

		private void setVrEstornado(final String vrEstornado) {
			this.vrEstornado = vrEstornado;
		}

		public final String getVrSaldo() {
			return vrSaldo;
		}

		private void setVrSaldo(final String vrSaldo) {
			this.vrSaldo = vrSaldo;
		}

		public final String getNuAgencia() {
			return nuAgencia;
		}

		private void setNuAgencia(final String nuAgencia) {
			this.nuAgencia = nuAgencia;
		}
	}
}
