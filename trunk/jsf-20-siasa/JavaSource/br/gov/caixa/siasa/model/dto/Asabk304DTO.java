package br.gov.caixa.siasa.model.dto;

import org.apache.commons.validator.GenericValidator;
import org.apache.log4j.Logger;

import br.gov.caixa.siasa.exception.InvalidFieldException;

/*
01 ASABK304-AREA.
03 NU-TAMANHO-NEG-BK304                      PIC 9(005).
03 CO-REGRA-NEG-BK304                        PIC X(005).
03 DADOS-CONTROLE-BK304.
   05 CO-RETORNO-WBLB-BK304                  PIC 9(003).
   05 DE-MENSAGEM-WBLB-BK304                 PIC X(042).
03 DADOS-ENTRADA-BK304 REDEFINES DADOS-CONTROLE-BK304.
   05 CO-USUARIO-ENT-BK304                   PIC X(008).
   05 LI-CERTIFICACAO-ENT-BK304              PIC X(012).
   05 NU-DOACAO-BK304                        PIC 9(012).
   05 CO-ESTORNO-BK304                       PIC 9(009).
   05 NU-AGENCIA-BK304                       PIC 9(004).
03 CODIGOS-DE-RETORNO.
   05 CO-RETORNO-BK304                       PIC 9(001).
   05 DE-MENSAGEM-BK304                      PIC X(080).
   05 CO-ERRO-SQL-BK304                      PIC X(004).
*/   
public final class Asabk304DTO extends CobolBook {

	private static final long serialVersionUID = -1671147074699694086L;
	private static final Logger logger = Logger.getLogger(Asabk304DTO.class);
//	ENTRADA
	private String nuDoacao;
	private String coEstorno;
	private String nuAgencia;
	
	@Override
	public String toCICS() {
		if(GenericValidator.isBlankOrNull(getNuDoacao())) {
			setNuDoacao("0");
		}
		if(GenericValidator.isBlankOrNull(getCoEstorno())) {
			setCoEstorno("0");
		}
		if(GenericValidator.isBlankOrNull(getNuAgencia())) {
			setNuAgencia("0");
		}
		StringBuilder builder = new StringBuilder();
		builder.append(getCoUsuario());
		builder.append(getDeCertificacao());
		builder.append(pic(Long.parseLong(getNuDoacao()),12));
		builder.append(pic(Long.parseLong(getCoEstorno()),9));
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
			setCoRetorno(bigString.substring(beginIndex, beginIndex+1));
			logger.debug("coRetorno ["+getCoRetorno()+"]");
			beginIndex =+ 1;
			setDeMensagem(bigString.substring(beginIndex, beginIndex+80));
			logger.debug("deMensagem ["+getDeMensagem()+"]");
			beginIndex =+ 80;
			setCoSqlcode(bigString.substring(beginIndex, beginIndex+4));
			logger.debug("coSqlcode ["+getCoSqlcode()+"]");
			beginIndex =+ 4;
		} catch (IndexOutOfBoundsException e) {
			logger.error("Tripão inválido: " + bigString, e);
			throw new InvalidFieldException("Tripão devolvido pelo CICS com tamanho inválido", e);
		} catch (NumberFormatException e) {
			logger.error("Tripão inválido: " + bigString, e);
			throw new InvalidFieldException("Tripão devolvido pelo CICS contém dados com valor inesperado",e);
		}
	}

	public final String getNuDoacao() {
		return nuDoacao;
	}

	public void setNuDoacao(final String nuDoacao) {
		this.nuDoacao = nuDoacao;
	}

	public final String getCoEstorno() {
		return coEstorno;
	}

	public void setCoEstorno(final String coEstorno) {
		this.coEstorno = coEstorno;
	}

	public final String getNuAgencia() {
		return nuAgencia;
	}

	public void setNuAgencia(final String nuAgencia) {
		this.nuAgencia = nuAgencia;
	}

}
