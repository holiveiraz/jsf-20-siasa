package br.gov.caixa.siasa.model.dto;

import java.util.ArrayList;
import java.util.List;

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
	/**
	 * 
	 */
	private static final long serialVersionUID = -7355834437852141240L;
	private String dtDoacao;
	private String dtCompensacao;
	private String dtRemessa;
	private String nuOcorrencias;
	private List<Asabk301Lista> ocorrencias;


	public Asabk301DTO() {
		this.ocorrencias = new ArrayList<Asabk301Lista>();
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

	public final String getNuOcorrencias() {
		return nuOcorrencias;
	}

	public void setNuOcorrencias(final String nuOcorrencias) {
		this.nuOcorrencias = nuOcorrencias;
	}

	public final List<Asabk301Lista> getOcorrencias() {
		return ocorrencias;
	}

	public void setOcorrencias(final List<Asabk301Lista> ocorrencias) {
		this.ocorrencias = ocorrencias;
	}

	public class Asabk301Lista {
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
			//TODO
		}
	}

	@Override
	public String toCICS() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void fromCICS(String bigString) {
		// TODO Auto-generated method stub
		
	}
}
