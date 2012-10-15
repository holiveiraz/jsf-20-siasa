package br.gov.caixa.siasa;

import br.gov.caixa.siasa.facade.FacadeImpl;
import br.gov.caixa.siasa.facade.IFacade;
import br.gov.caixa.siasa.model.dto.Asabk304DTO;
import br.gov.caixa.siasa.model.dto.Asabk308DTO;

public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		IFacade facade = new FacadeImpl();
		Asabk304DTO asabk304 = new Asabk304DTO();
		asabk304.setCoEstorno("123");
		asabk304.setCoUsuario("F928658");
		asabk304.setDeCertificacao("123456789012");
		asabk304.setNuAgencia("0198");
		asabk304.setNuDoacao("123456");
		facade.delecaoEstornoJaComandado(asabk304);
		
		Asabk308DTO asabk308 = new Asabk308DTO();
		asabk308.setCoUsuario("F928658");
		asabk308.setDeCertificacao("123456789012");
		asabk308.setCoTipoCanal("1");
		asabk308.setDeTipoCanal("TIPO CANAL 1");
		asabk308.setIcAcao("I");
		asabk308.setIcFase("3");
		facade.manutencaoTipoCancal(asabk308);
	}

}
