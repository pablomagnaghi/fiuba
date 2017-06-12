package fiuba.algo3.tp1;

import java.util.ArrayList;
import java.util.List;

public class ChatsGrupales extends Chats {

	private List<Grupo> grupos;

	public ChatsGrupales() {
		grupos = new ArrayList<Grupo>();
	}

	public int cantidadDeGrupos() {
		// TODO Auto-generated method stub
		return 0;
	}
	
	public void crearGrupo(String nombreGrupo) {
		// TODO Auto-generated method stub
		
	}

	public void agregarContactoAGrupo(String nombreGrupo, String nombreContacto) {
		// TODO Auto-generated method stub
		
	}

	public int cantidadMiembrosEnGrupo(String nombreGrupo) {
		// TODO Auto-generated method stub
		return 0;
	}

	public void recibirMensajeDeGrupo(String nombreGrupo, String nombreContacto, String mensaje) {
		// TODO Auto-generated method stub
		
	}

	public int cantidadMensajesRecibidosDelGrupo(String nombreGrupo) {
		// TODO Auto-generated method stub
		return 0;
	}

	public int cantidadMensajesEnviadosAlGrupo(String nombreGrupo) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int cantidadDeChats() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int cantidadDeMensajesEnviados() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int cantidadTotalMensajesRecibidos() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int cantidadTotalMensajesEnviados() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public boolean existeChat(String nombreChat) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public List<String> obtenerConversacionCon(String nombreChat) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void enviarMensajeA(String nombreChat, String mensaje) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void borrarMensajes(String nombreChat) {
		// TODO Auto-generated method stub
		
	}


	
	
}
