package br.gov.caixa.siasa.model.dao;

import br.gov.caixa.psc.connector.client.ClientFactory;
import br.gov.caixa.psc.connector.client.JConnectorClient;
import br.gov.caixa.psc.connector.client.JConnectorException;
import br.gov.caixa.psc.connector.model.ParametroInvalidoException;
import br.gov.caixa.psc.connector.model.pscpslnk.PSCAreaDeComunicacao;
import br.gov.caixa.psc.connector.model.pscpslnk.PSCAreaDeControle;
import br.gov.caixa.psc.connector.model.pscpslnk.PSCAreaDeDados;
import br.gov.caixa.psc.connector.model.pscpslnk.PSCAreaDeEntrada;
import br.gov.caixa.psc.connector.model.pscpslnk.PSCAreaDeSaida;
import br.gov.caixa.psc.connector.model.types.AreaPassarPrograma;
import br.gov.caixa.psc.connector.model.types.FormaChamarPrograma;
import br.gov.caixa.siasa.exception.DAOException;
import br.gov.caixa.siasa.model.dto.CobolBook;

public final class CicsDAO implements IDao {

	private static final String target = "CICSASA";
	private String noPrograma;
	
	public CicsDAO(String noPrograma) {
		setNoPrograma(noPrograma);
	}

	private void setNoPrograma(String noPrograma) {
		this.noPrograma = noPrograma;
	}

	@Override
	public CobolBook execute(CobolBook cb) {
		try {
			// ---- Criando instancia do JConnector ----
			JConnectorClient connectorDAO = ClientFactory.getInstance().getClient(target);

			// --- Controle ---
			final PSCAreaDeControle areaControle = new PSCAreaDeControle( );
			areaControle.setSistema("SIASA");
			areaControle.setNomePrograma(this.noPrograma);
			areaControle.setFormaChamarPrograma(FormaChamarPrograma.LINK );
			areaControle.setAreaParaPassarAoPrograma(AreaPassarPrograma.SOMENTE_DADOS);

			// --- Area de Dados ---
			final PSCAreaDeDados areaDeDados = new PSCAreaDeDados(new PSCAreaDeEntrada("", cb.toCICS()));//passa o numero para o cisc
			final PSCAreaDeComunicacao areaDeComunicacao = new PSCAreaDeComunicacao( areaControle, areaDeDados);
			final PSCAreaDeSaida areaDeSaida = (PSCAreaDeSaida) connectorDAO.executeCicsTransaction(areaDeComunicacao);

			if (areaDeSaida.hasErro()) {
				throw new DAOException(areaDeSaida.getCodigoErro() + " - "+ areaDeSaida.getDescricaoErro());
			}

			cb.fromCICS(areaDeSaida.getAreaDeSaidaMessageWithoutCodigoRegraNegocio());//area de saida

		} catch (ParametroInvalidoException e) {
			throw new DAOException(e);
		} catch (JConnectorException e) {
			throw new DAOException(e);
		}
		return cb;	
	}

}
