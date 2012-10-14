package br.gov.caixa.siasa.model.dto;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.validator.GenericValidator;
import org.apache.log4j.Logger;

import br.gov.caixa.siasa.exception.InvalidFieldException;

/*
 01 ASABK301-AREA.
 03 NU-TAMANHO-NEG-BK301                      PIC 9(005).
 03 CO-REGRA-NEG-BK301                        PIC X(005).
 03 DADOS-CONTROLE-BK301.
 05 CO-RETORNO-WBLB-BK301                  PIC 9(003).
 05 DE-MENSAGEM-WBLB-BK301                 PIC X(047).
 03 DADOS-ENTRADA-BK301 REDEFINES DADOS-CONTROLE-BK301.
 05 DT-DOACAO-ENT-BK301                    PIC X(010).
 05 DT-COMPENSACAO-ENT-BK301               PIC X(010).
 05 DT-REMESSA-ENT-BK301                   PIC X(010).
 05 CO-USUARIO-ENT-BK301                   PIC X(008).
 05 LI-CERTIFICACAO-ENT-BK301              PIC X(012).
 03 DADOS-SAIDA-BK301.
 07 NU-OCORRENCIAS-BK301                   PIC 9(003).
 07 TAB-SAI-BK301  OCCURS  30  TIMES.
 10 NU-DOACAO-BK301                     PIC 9(012).
 10 NU-CPF-BK301                        PIC 9(011).
 10 NU-CNPJ-BK301                       PIC 9(014).
 10 DT-DOACAO-BK301                     PIC X(010).
 10 DT-COMPENSACAO-BK301                PIC X(010).
 10 DT-REMESSA-BK301                    PIC X(010).
 10 DT-PROCESSAMENTO-BK301              PIC X(010).
 10 CO-FINANCEIRO-BK301                 PIC 9(009).
 10 QT-DIA-BLOQUEIO-BK301               PIC 9(004).
 10 DT-ESTORNO-BK301                    PIC X(010).
 10 VR-DOACAO-BK301                     PIC 9(013)V9(02).
 10 VR-ESTORNADO-BK301                  PIC 9(013)V9(02).
 10 NU-AGENCIA-BK301                    PIC 9(004).
 07 CODIGOS-DE-RETORNO.
 10 CO-RETORNO-BK301                    PIC 9(001).
 10 DE-MENSAGEM-BK301                   PIC X(080).
 10 CO-ERRO-SQL-BK301                   PIC X(004).
 */
public final class Asabk301DTO extends CobolBook {

	private static final long serialVersionUID = -7355834437852141240L;
	private static final Logger logger = Logger.getLogger(Asabk301DTO.class);
//	ENTRADA
	private String dtDoacao;
	private String dtCompensacao;
	private String dtRemessa;
//	SAIDA
	private String nuOcorrencia;
	private List<DoacaoCheque> doacaoCheque;

	public Asabk301DTO() {
		this.doacaoCheque = new ArrayList<DoacaoCheque>();
	}

	@Override
	public String toCICS() {
		if(GenericValidator.isBlankOrNull(getDtRemessa())) {
			setDtRemessa(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtCompensacao())) {
			setDtCompensacao(DATA_ZERADA);
		}
		if(GenericValidator.isBlankOrNull(getDtDoacao())) {
			setDtDoacao(DATA_ZERADA);
		}
		
		StringBuilder builder = new StringBuilder();
		builder.append(getDtDoacao());
		builder.append(getDtCompensacao());
		builder.append(getDtRemessa());
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
				final String littleString = bigString.substring(beginIndex, beginIndex + 134);
				final DoacaoCheque lancamento = new DoacaoCheque();
				lancamento.fromCICS(littleString);
				getDoacaoCheque().add(lancamento);
				beginIndex += 134;
			}
			beginIndex += (134 * 30) - (134 * x);
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

	public final String getDtRemessa() {
		return dtRemessa;
	}

	public void setDtRemessa(final String dtRemessa) {
		this.dtRemessa = dtRemessa;
	}

	public final String getNuOcorrencia() {
		return nuOcorrencia;
	}

	public void setNuOcorrencia(final String nuOcorrencia) {
		this.nuOcorrencia = nuOcorrencia;
	}

	public List<DoacaoCheque> getDoacaoCheque() {
		return doacaoCheque;
	}

	public void setDoacaoCheque(List<DoacaoCheque> doacaoCheque) {
		this.doacaoCheque = doacaoCheque;
	}

	public final class DoacaoCheque {
		private String nuDoacao;
		private String nuCPF;
		private String nuCPNJ;
		private String dtDoacao;
		private String dtCompensacao;
		private String dtRemessa;
		private String dtProcessamento;
		private String coFinanceiro;
		private String qtDiaBloqueio;
		private String dtEstorno;
		private String vrDoacao;
		private String vrEstornado;
		private String nuAgencia;

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

		public final String getNuCPNJ() {
			return nuCPNJ;
		}

		private void setNuCPNJ(final String nuCPNJ) {
			this.nuCPNJ = nuCPNJ;
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

		public final String getDtProcessamento() {
			return dtProcessamento;
		}

		private void setDtProcessamento(final String dtProcessamento) {
			this.dtProcessamento = dtProcessamento;
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
				setNuCPF(littleString.substring(beginIndex, beginIndex + 11));
				logger.debug("nuCPF ["+getNuCPF()+"]");
				beginIndex += 11;
				setNuCPNJ(littleString.substring(beginIndex, beginIndex + 14));
				logger.debug("nuCNPJ ["+getNuCPNJ()+"]");
				beginIndex += 14;
				setDtDoacao(littleString.substring(beginIndex, beginIndex + 10));
				logger.debug("dtDoacao ["+getDtDoacao()+"]");
				beginIndex += 10;
				setDtCompensacao(littleString.substring(beginIndex,beginIndex + 10));
				logger.debug("dtCompensacao ["+getDtCompensacao()+"]");
				beginIndex += 10;
				setDtRemessa(littleString.substring(beginIndex, beginIndex + 10));
				logger.debug("dtRemessa ["+getDtRemessa()+"]");
				beginIndex += 10;
				setDtProcessamento(littleString.substring(beginIndex,beginIndex + 10));
				logger.debug("dtProcessamento ["+getDtProcessamento()+"]");
				beginIndex += 10;
				setCoFinanceiro(littleString.substring(beginIndex,beginIndex + 9));
				logger.debug("coFinanceir ["+getCoFinanceiro()+"]");
				beginIndex += 9;
				setQtDiaBloqueio(littleString.substring(beginIndex,beginIndex + 4));
				logger.debug("qtDiaBloqueio ["+getQtDiaBloqueio()+"]");
				beginIndex += 4;
				setDtEstorno(littleString.substring(beginIndex, beginIndex + 10));
				logger.debug("dtEstorno ["+getDtEstorno()+"]");
				beginIndex += 10;
				setVrDoacao(littleString.substring(beginIndex, beginIndex + 15));
				logger.debug("vrDoacao ["+getVrDoacao()+"]");
				beginIndex += 15;
				setVrEstornado(littleString.substring(beginIndex,beginIndex + 15));
				logger.debug("vrEstornado ["+getVrEstornado()+"]");
				beginIndex += 15;
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
