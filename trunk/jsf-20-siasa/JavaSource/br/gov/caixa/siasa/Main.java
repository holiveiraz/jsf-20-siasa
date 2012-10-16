package br.gov.caixa.siasa;

import br.gov.caixa.siasa.facade.FacadeImpl;
import br.gov.caixa.siasa.facade.IFacade;
import br.gov.caixa.siasa.model.dto.Asabk301DTO;
import br.gov.caixa.siasa.model.dto.Asabk302DTO;
import br.gov.caixa.siasa.model.dto.Asabk303DTO;
import br.gov.caixa.siasa.model.dto.Asabk304DTO;
import br.gov.caixa.siasa.model.dto.Asabk305DTO;
import br.gov.caixa.siasa.model.dto.Asabk306DTO;
import br.gov.caixa.siasa.model.dto.Asabk307DTO;
import br.gov.caixa.siasa.model.dto.Asabk308DTO;

public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		IFacade facade = new FacadeImpl();
		Asabk301DTO asabk301 = new Asabk301DTO();
		asabk301.setDtDoacao("01/01/0001");
		asabk301.setDtCompensacao("02/02/0002");
		asabk301.setDtRemessa("03/03/0003");
		asabk301.setCoUsuario("F928658");
		asabk301.setDeCertificacao("123456789012");
		facade.consultaLancamentoDoacaoEmCheque(asabk301);

		Asabk302DTO asabk302 = new Asabk302DTO();
		asabk302.setCoUsuario("F928658");
		asabk302.setDeCertificacao("123456789012");
		asabk302.setDtDoacao("01/01/0001");
		asabk302.setDtCompensacao("02/02/0002");
		asabk302.setDtApuracao("03/03/0003");
		asabk302.setDtProcessamento("04/04/0004");
		asabk302.setNuDoacao("001");
		asabk302.setVrDoacao("999900");
		asabk302.setQtDiaBloqueio("2");
		asabk302.setDtDeRemeter("05/05/0005");
		asabk302.setVrDoacao("888800");
		asabk302.setNuAgencia("4064");
		facade.consultaLancamentoEstornoPendente(asabk302);
		System.exit(0);		
		Asabk303DTO asabk303 = new Asabk303DTO();
		asabk303.setCoUsuario("F928658");
		asabk303.setDeCertificacao("123456789012");
		facade.inclusaoEstornoCheque(asabk303);
		
		Asabk304DTO asabk304 = new Asabk304DTO();
		asabk304.setCoEstorno("123");
		asabk304.setCoUsuario("F928658");
		asabk304.setDeCertificacao("123456789012");
		asabk304.setNuAgencia("0198");
		asabk304.setNuDoacao("123456");
		facade.delecaoEstornoJaComandado(asabk304);
		
		Asabk305DTO asabk305 = new Asabk305DTO();
		asabk305.setDtDoacaoDe("01/01/0001");
		asabk305.setDtDoacaoAte("31/01/0001");
		asabk305.setDtCompensacaoDe("01/01/0001");
		asabk305.setDtCompensacaoAte("31/01/0001");
		asabk305.setDtRemessaDe("01/01/0001");
		asabk305.setDtRemessaAte("31/01/0001");
		asabk305.setCoUsuario("F928658");
		asabk305.setDeCertificacao("123456789012");
		facade.consultaLancamentoCheque(asabk305);

		Asabk306DTO asabk306 = new Asabk306DTO();
		asabk306.setDtApuracaoDe("01/01/0001");
		asabk306.setDtApuracaoAte("31/01/0001");
		asabk306.setNuApuracao("900");
		asabk306.setCoUsuario("F928658");
		asabk306.setDeCertificacao("123456789012");
		facade.consultaApuracaoRealizada(asabk306);

		Asabk307DTO asabk307 = new Asabk307DTO();
		asabk307.setDtRemessaDe("01/01/0001");
		asabk307.setDtRemessaAte("31/01/0001");
		asabk307.setNuRemessa("015");
		asabk307.setCoUsuario("F928658");
		asabk307.setDeCertificacao("123456789012");
		facade.consultaRemessaJaRealizada(asabk307);
		
		Asabk308DTO asabk308 = new Asabk308DTO();
		asabk308.setCoUsuario("F928658");
		asabk308.setDeCertificacao("123456789012");
		asabk308.setCoTipoCanal("1");
		asabk308.setDeTipoCanal("TIPO CANAL 1");
		asabk308.setIcAcao("I");
		asabk308.setIcFase("3");
		facade.manutencaoTipoCanal(asabk308);
	}

}
